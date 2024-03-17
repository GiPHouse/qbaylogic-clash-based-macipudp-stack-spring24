{-# language RecordWildCards #-}

module Clash.Cores.Ethernet.DownConverter
  ( downConverter
  , downConverterC
  , sampleOut
  ) where

import Clash.Cores.Ethernet.PacketStream
import Clash.Prelude
import Data.List qualified as L
import Data.Maybe ( isJust, isNothing )
import Protocols ( Circuit, fromSignals )

data DownConverterState (dataWidth :: Nat) =
  DownConverterState {
    _dcBuf :: Vec dataWidth (BitVector 8),
    -- ^ Buffer
    _dcSize :: Index (dataWidth + 1),
    -- ^ Number of valid bytes in _dcBuf
    _dcLastVec :: Bool,
    -- ^ True if last byte of _dcBuf was marked as last byte by incoming stream
    _dcAborted :: Bool
    -- ^ If True, outgoing bytes should be marked as aborted until _dcBuf is replaced
  }
  deriving (Generic, NFDataX)

-- | Maybe put this in a utility module?
toMaybe :: Bool -> a -> Maybe a
toMaybe True x = Just x
toMaybe False _ = Nothing

-- | Computes new state from incoming data
fromPacketStreamM2S
  :: forall (dataWidth :: Nat) .
  KnownNat dataWidth
  => PacketStreamM2S dataWidth ()
  -> DownConverterState dataWidth
fromPacketStreamM2S (PacketStreamM2S vs lastIdx _ aborted) =
  DownConverterState
    { _dcBuf = vs
    , _dcSize = maybe (natToNum @dataWidth) (succ . resize) lastIdx -- lastIdx points to the last valid byte, so the buffer size is one more
    , _dcLastVec = isJust lastIdx
    , _dcAborted = aborted
    }

-- | Computes output of down converter
toMaybePacketStreamM2S
  :: forall (dataWidth :: Nat) .
  1 <= dataWidth
  => KnownNat dataWidth
  => DownConverterState dataWidth
  -> Maybe (PacketStreamM2S 1 ())
toMaybePacketStreamM2S DownConverterState {..} = toMaybe (_dcSize > 0) out
  where
    out = PacketStreamM2S
      { _data = leToPlusKN @1 @dataWidth head _dcBuf :> Nil
      , _last = toMaybe (_dcSize == 1 && _dcLastVec) 0
      , _meta = ()
      , _abort = _dcAborted
      }

downConverter
  :: forall (dataWidth :: Nat) (dom :: Domain).
  HiddenClockResetEnable dom
  => 1 <= dataWidth
  => KnownNat dataWidth
  => ( Signal dom (Maybe (PacketStreamM2S dataWidth ()))
     , Signal dom PacketStreamS2M
     )
  -- ^ Input packet stream from the source and backpressure from the sink
  -> ( Signal dom PacketStreamS2M
     , Signal dom (Maybe (PacketStreamM2S 1 ()))
     )
  -- ^ Output backpressure to the source
  --   Output packet stream to the sink
downConverter = mealyB go s0
  where
    s0 = DownConverterState
      { _dcBuf = errorX "downConverter: undefined initial value"
      , _dcSize = 0
      , _dcLastVec = False
      , _dcAborted = False
      }
    go
      :: DownConverterState dataWidth
      -> (Maybe (PacketStreamM2S dataWidth ()), PacketStreamS2M)
      -> (DownConverterState dataWidth, (PacketStreamS2M, Maybe (PacketStreamM2S 1 ())))
    go st@(DownConverterState {..}) (fwdIn, PacketStreamS2M inReady) = (st', (bwdOut, fwdOut))
      where
        -- Compute next buffer and its size. If a byte was just acknowledged,
        -- a byte is removed. Otherwise, it is left unchanged.
        (_buf', _size') = if True && _dcSize > 0
          then (_dcBuf <<+ 0, _dcSize - 1)
          else (_dcBuf, _dcSize)
        -- If our next buffer will be empty, we are ready to receive new data,
        -- and if there is valid data already, put it in a fresh state.
        -- Otherwise, keep the current state with the new buffer and size.
        outReady = _size' == 0
        st' = case (outReady, fwdIn) of
          (True, Just packetStream) -> fromPacketStreamM2S packetStream
          _                         -> st { _dcBuf = _buf', _dcSize = _size' }
        bwdOut = PacketStreamS2M outReady
        fwdOut = toMaybePacketStreamM2S st

downConverterC
  :: forall (dataWidth :: Nat) (dom :: Domain).
  HiddenClockResetEnable dom
  => 1 <= dataWidth
  => KnownNat dataWidth
  => Circuit (PacketStream dom dataWidth ()) (PacketStream dom 1 ())
downConverterC = fromSignals downConverter

payloadInp :: [Maybe (PacketStreamM2S 4 ())]
payloadInp = [
  Nothing
  , Just (PacketStreamM2S (0x01 :> 0x02 :> 0x03 :> 0x04 :> Nil) Nothing () False)
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Just (PacketStreamM2S (0xC0 :> 0xFF :> 0xEE :> 0x00 :> Nil) (Just 2) () False)
  , Nothing
  , Nothing
  , Just (PacketStreamM2S (0xDE :> 0xAD :> 0x00 :> 0x00 :> Nil) (Just 1) () True)
  , Just (PacketStreamM2S (0xDE :> 0xAD :> 0x00 :> 0x00 :> Nil) (Just 1) () True)
  , Nothing
  , Nothing
  , Just (PacketStreamM2S (0xAA :> 0xAA :> 0xAA :> 0xAA :> Nil) Nothing () False)
  , Just (PacketStreamM2S (0xAA :> 0xAA :> 0xAA :> 0xAA :> Nil) Nothing () False)
  , Just (PacketStreamM2S (0x55 :> 0x55 :> 0x55 :> 0x55 :> Nil) (Just 3) () False)
  , Just (PacketStreamM2S (0x55 :> 0x55 :> 0x55 :> 0x55 :> Nil) (Just 3) () False)
  , Just (PacketStreamM2S (0x55 :> 0x55 :> 0x55 :> 0x55 :> Nil) (Just 3) () False)
  , Just (PacketStreamM2S (0x55 :> 0x55 :> 0x55 :> 0x55 :> Nil) (Just 3) () False)
  ] L.++ L.repeat Nothing

sinkReadyInp :: [PacketStreamS2M]
sinkReadyInp = fmap PacketStreamS2M ((L.repeat True))

clk :: Clock System
clk = systemClockGen

rst :: Reset System
rst = systemResetGen

en :: Enable dom
en = enableGen

payloadOut :: Signal System (Maybe (PacketStreamM2S 1 ()))
sinkReadyOut :: Signal System PacketStreamS2M
downConverterClk ::
 (Signal System (Maybe (PacketStreamM2S 4 ()))
 , Signal System PacketStreamS2M)
  -> (Signal System PacketStreamS2M
     , Signal System (Maybe (PacketStreamM2S 1 ())))
downConverterClk = exposeClockResetEnable downConverter clk rst en

(sinkReadyOut, payloadOut) = downConverterClk (fromList $ [Nothing, Nothing, Nothing] L.++ fmap Just inp', fromList sinkReadyInp)

sampleOut :: [(Maybe (PacketStreamM2S 1 ()), PacketStreamS2M)]
sampleOut = sampleN 26 $ bundle (payloadOut, sinkReadyOut)


inp' :: [PacketStreamM2S 4 ()]
inp' = [
  PacketStreamM2S {
    _abort = False
    ,_last  = Just 2
    ,_meta  = ()
    ,_data  = 1 :> 2 :> 3 :> 4 :> Nil
  }
  , PacketStreamM2S {
    _abort = False
    ,_last  = Nothing
    ,_meta  = ()
    ,_data  = 5 :> 6 :> 7 :> 8 :> Nil
  }]

inp :: [PacketStreamM2S 4 ()]
inp = [
  PacketStreamM2S {
    _abort = False
    ,_last  = Just 2
    ,_meta  = ()
    ,_data  = pure 1
  }
  , PacketStreamM2S {
    _abort = False
    ,_last  = Nothing
    ,_meta  = ()
    ,_data  = pure 1
  }
  , PacketStreamM2S {
    _abort = True
    ,_last  = Just 3
    ,_meta  = ()
    ,_data  = pure 2
  }
  , PacketStreamM2S {
    _abort = False
    ,_last  = Nothing
    ,_meta  = ()
    ,_data  = pure 3
  }
  , PacketStreamM2S {
    _abort = False
    ,_last  = Nothing
    ,_meta  = ()
    ,_data  = pure 4
  }
  , PacketStreamM2S {
    _abort = False
    ,_last  = Nothing
    ,_meta  = ()
    ,_data  = pure 5
  }
  , PacketStreamM2S {
    _abort = False
    ,_last  = Nothing
    ,_meta  = ()
    ,_data  = pure 6
  }
  , PacketStreamM2S {
    _abort = True
    ,_last  = Just 0
    ,_meta  = ()
    ,_data  = pure 7
  }
  , PacketStreamM2S {
    _abort = False
    ,_last  = Nothing
    ,_meta  = ()
    ,_data  = pure 8
  }
  , PacketStreamM2S {
    _abort = True
    ,_last  = Just 0
    ,_meta  = ()
    ,_data  = pure 9
  }]


chopPacket :: forall n. 1 <= n => KnownNat n => PacketStreamM2S n () -> [PacketStreamM2S 1 ()]
chopPacket PacketStreamM2S {..} = packets where
  lasts = case _last of
    Nothing  -> L.repeat Nothing
    Just in' -> L.replicate (fromIntegral in') Nothing L.++ [Just (0 :: Index 1) ]

  datas = case _last of
    Nothing -> toList _data
    Just in' -> L.take (fromIntegral in' + 1) $ toList _data

  packets = (\(idx,  dat) -> PacketStreamM2S (pure dat) idx () _abort) <$> L.zip lasts datas

model :: forall n. 1 <= n => KnownNat n => [PacketStreamM2S n ()] -> [PacketStreamM2S 1 ()]
model fragments = fragments >>= chopPacket

