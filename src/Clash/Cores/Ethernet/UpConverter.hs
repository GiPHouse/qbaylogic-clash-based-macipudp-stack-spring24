{-# language RecordWildCards #-}
module Clash.Cores.Ethernet.UpConverter
  ( upConverter
  , upConverterC
  , sampleOut
  ) where

import Clash.Prelude
import Data.Maybe ( isJust, isNothing )
import Control.Monad ((>>=))

import Clash.Cores.Ethernet.PacketStream

import Data.List qualified as L
import Protocols ( Circuit, fromSignals, (|>), Circuit(..))

forceResetSanity :: forall dom n meta. HiddenClockResetEnable dom => Circuit (PacketStream dom n meta) (PacketStream dom n meta)
forceResetSanity
  = Circuit (\(fwd, bwd) -> unbundle . fmap f . bundle $ (rstLow, fwd, bwd))
 where
  f (True,  _,   _  ) = (PacketStreamS2M False, Nothing)
  f (False, fwd, bwd) = (bwd, fwd)
  rstLow = unsafeToHighPolarity hasReset

data UpConverterState (dataWidth :: Nat) =
  UpConverterState {
    _ucBuf     :: Vec dataWidth (BitVector 8),
    -- ^ The buffer we are filling
    _ucIdx     :: Index dataWidth,
    -- ^ Where in the buffer we need to write the next element
    _ucFlush   :: Bool,
    -- ^ If this is true the current state can presented as packetstream word
    _ucAborted :: Bool,
    -- ^ Current packet is aborted
    _ucLastIdx :: Maybe (Index dataWidth)
    -- ^ If true the current buffer contains the last byte of the current packet
  }
  deriving (Generic, NFDataX)
-- ^ Upconverter state, consisting of at most p (BitVector 8)'s and a vector indicating which bytes are valid

-- | Maybe put this in a utility module?
toMaybe :: Bool -> a -> Maybe a
toMaybe True x = Just x
toMaybe False _ = Nothing

toPacketStream :: UpConverterState dataWidth -> Maybe (PacketStreamM2S dataWidth ())
toPacketStream UpConverterState{..} = toMaybe _ucFlush (PacketStreamM2S _ucBuf _ucLastIdx () _ucAborted)

upConverter
  :: forall (dataWidth :: Nat) (dom :: Domain).
  HiddenClockResetEnable dom
  => 1 <= dataWidth
  =>  KnownNat dataWidth
  => ( Signal dom (Maybe (PacketStreamM2S 1 ()))
     , Signal dom PacketStreamS2M)
  -- ^ Input packet stream from the source
  --   Input backpressure from the sink
  -> ( Signal dom PacketStreamS2M
     , Signal dom (Maybe (PacketStreamM2S dataWidth ()))
     )
  -- ^ Output backpressure to the source
  --   Output packet stream to the sink
upConverter = mealyB go s0
  where
    s0 = UpConverterState (repeat 0) 0 False False Nothing
    go
      :: UpConverterState dataWidth
      -> (Maybe (PacketStreamM2S 1 ()), PacketStreamS2M)
      -> ( UpConverterState dataWidth
         , (PacketStreamS2M, Maybe (PacketStreamM2S dataWidth ()))
         )
    go st@(UpConverterState {..}) (Nothing, PacketStreamS2M inReady)
      = (nextSt, (PacketStreamS2M outReady, toPacketStream st))
        where
          -- If we can accept data we can always set _ucFlush to false,
          -- since we only change state if we can transmit and receive data
          nextStRaw = st
                        { _ucFlush = False
                        , _ucAborted = isNothing _ucLastIdx && _ucAborted
                        , _ucLastIdx = Nothing
                        }
          outReady = not _ucFlush || inReady
          nextSt = if outReady then nextStRaw else st
    go st@(UpConverterState {..}) (Just (PacketStreamM2S{..}), PacketStreamS2M inReady)
      = (nextSt, (PacketStreamS2M outReady, toPacketStream st))
        where
          inLast = isJust _last
          -- We smear an abort over the entire rest of the packet
          -- so the next abort is set:
          --  - If fragment we are potentially flushing was not the last and we were already aborting;
          --  - or if the incoming fragment is aborted
          nextAbort = (isNothing _ucLastIdx && _ucAborted) || _abort
          -- If we are not flushing we can accept data to be stored in _ucBuf,
          -- but when we are flushing we can only accept if the current
          -- output fragment is accepted by the sink
          outReady = not _ucFlush || inReady
          bufFull = _ucIdx == maxBound
          nextBuf = replace _ucIdx (head _data) _ucBuf

          nextFlush = inLast || bufFull
          nextIdx = if nextFlush then 0 else _ucIdx + 1

          nextStRaw = UpConverterState
                        { _ucBuf =  nextBuf
                        , _ucIdx = nextIdx
                        , _ucFlush = nextFlush
                        , _ucAborted = nextAbort
                        , _ucLastIdx = toMaybe inLast _ucIdx
                        }
          nextSt = if outReady then nextStRaw else st

upConverterC
  :: forall (dataWidth :: Nat) (dom :: Domain).
  HiddenClockResetEnable dom
  => 1 <= dataWidth
  => KnownNat dataWidth
  => Circuit (PacketStream dom 1 ()) (PacketStream dom dataWidth ())
upConverterC = forceResetSanity |> fromSignals upConverter

payloadInp :: [Maybe (PacketStreamM2S 1 ())]
payloadInp = [
  Nothing
  , Just (PacketStreamM2S (0x01 :> Nil) Nothing () False)
  , Just (PacketStreamM2S (0x02 :> Nil) (Nothing) () False)
  , Just (PacketStreamM2S (0x03 :> Nil) Nothing () False)
  , Nothing
  , Just (PacketStreamM2S (0x04 :> Nil) (Nothing) () False)
  , Just (PacketStreamM2S (0x14 :> Nil) (Just 0) () False)
  , Just (PacketStreamM2S (0x15 :> Nil) (Just 0) () False)
  , Just (PacketStreamM2S (0x16 :> Nil) (Just 0) () False)
  , Nothing
  , Just (PacketStreamM2S (0x05 :> Nil) Nothing () False)
  , Just (PacketStreamM2S (0x06 :> Nil) Nothing () False)
  , Nothing
  , Just (PacketStreamM2S (0x07 :> Nil) (Just 0) () True)
  ] L.++ (L.repeat Nothing)

sinkReadyInp :: [PacketStreamS2M]
sinkReadyInp = fmap PacketStreamS2M ([False, True, True, True, True, True, True] L.++ (L.repeat True))

clk :: Clock System
clk = systemClockGen

rst :: Reset System
rst = systemResetGen

en :: Enable dom
en = enableGen

payloadOut :: Signal System (Maybe (PacketStreamM2S 4 ()))
sinkReadyOut :: Signal System PacketStreamS2M
upConverterClk = exposeClockResetEnable (upConverter @4) clk rst en
(sinkReadyOut, payloadOut) = upConverterClk (fromList payloadInp, fromList sinkReadyInp)

sampleOut = sampleN 20 $ bundle (payloadOut, sinkReadyOut)


chunkBy :: (a -> Bool) -> [a] -> [[a]]
chunkBy _ [] = []
chunkBy predicate list = L.filter (not . null) $ chunkByHelper predicate list []

-- Helper function to accumulate chunks
chunkByHelper :: (a -> Bool) -> [a] -> [a] -> [[a]]
chunkByHelper _ [] acc = [L.reverse acc]
chunkByHelper predicate (x : xs) acc
  | predicate x = L.reverse (x : acc) : chunkByHelper predicate xs []
  | otherwise = chunkByHelper predicate xs (x : acc)

smearAbort :: [PacketStreamM2S n meta] -> [PacketStreamM2S n meta]
smearAbort [] = []
smearAbort (x:xs) = L.reverse $ L.foldl' go [x] xs
  where
    go l@(a:as) (PacketStreamM2S dat last meta abort)
      = (PacketStreamM2S dat last meta (_abort a || abort)):l

chopBy :: Int -> [a] -> [[a]]
chopBy chopsize list = L.filter (not . null) $ chopByHelper 0 list []
  where
    chopByHelper _ [] acc = [L.reverse acc]
    chopByHelper counter l@(a:as) acc 
      | counter == chopsize = (L.reverse acc):chopByHelper 0 l []
      | otherwise           = chopByHelper (counter + 1) as (a:acc)

chunkToPacket :: [PacketStreamM2S 1 ()] -> PacketStreamM2S 4 ()
chunkToPacket l = PacketStreamM2S {
    _last = if isJust $ _last $ L.last l then Just (fromIntegral $ L.length l - 1) else Nothing
  , _abort = or $ fmap _abort l
  , _meta = ()
  , _data = L.foldr (+>>) (repeat 0 :: Vec 4 (BitVector 8)) $ fmap (head . _data) l

}

inp :: [PacketStreamM2S 1 ()]
inp = [
  PacketStreamM2S {
    _abort = False
    ,_last  = Just (fromIntegral 0)
    ,_meta  = ()
    ,_data  = singleton 1
  }
  , PacketStreamM2S {
    _abort = False
    ,_last  = Nothing
    ,_meta  = ()
    ,_data  = singleton 1
  }
  , PacketStreamM2S {
    _abort = True
    ,_last  = Just (fromIntegral 0)
    ,_meta  = ()
    ,_data  = singleton 2
  }
  , PacketStreamM2S {
    _abort = False
    ,_last  = Nothing
    ,_meta  = ()
    ,_data  = singleton 3
  }
  , PacketStreamM2S {
    _abort = False
    ,_last  = Nothing
    ,_meta  = ()
    ,_data  = singleton 4
  }
  , PacketStreamM2S {
    _abort = False
    ,_last  = Nothing
    ,_meta  = ()
    ,_data  = singleton 5
  }
  , PacketStreamM2S {
    _abort = False
    ,_last  = Nothing
    ,_meta  = ()
    ,_data  = singleton 6
  }
  , PacketStreamM2S {
    _abort = True
    ,_last  = Just (fromIntegral 0)
    ,_meta  = ()
    ,_data  = singleton 7
  }
  , PacketStreamM2S {
    _abort = False
    ,_last  = Nothing
    ,_meta  = ()
    ,_data  = singleton 8
  }
  , PacketStreamM2S {
    _abort = True
    ,_last  = Just (fromIntegral 0)
    ,_meta  = ()
    ,_data  = singleton 9
  }]

res = chunkBy id [False, False, True, False, True, True, True, False, False]

model :: [PacketStreamM2S 1 ()] -> [PacketStreamM2S 4 ()]
model fragments = out
  where
    wholePackets = (fmap smearAbort $ chunkBy (isJust . _last) fragments)
    chunks = wholePackets >>= (chopBy 4)
    out    =  chunkToPacket chunks
