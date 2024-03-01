{-# LANGUAGE RecordWildCards #-}

module Clash.Cores.Ethernet.DownConverter
  ( downConverter
  , sampleOut
  ) where

import Clash.Prelude
import Data.Maybe ( isJust )
import Clash.Cores.Ethernet.PacketStream
import qualified Data.List as L

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
    , _dcSize = case lastIdx of
                Just n -> resize n + 1 -- lastIdx points to the last valid byte, so the buffer size is one more
                Nothing -> natToNum @dataWidth
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
toMaybePacketStreamM2S DownConverterState {..} = toMaybe (_dcSize == 0) out
  where
    out = PacketStreamM2S
      { _data = leToPlusKN @1 @dataWidth head _dcBuf :> Nil
      , _last = toMaybe (_dcSize == 1 && _dcLastVec) 0
      , _meta = ()
      , _abort = _dcAborted
      }

downConverter
  :: forall (dom :: Domain).
  HiddenClockResetEnable dom
  => Signal dom (Maybe (PacketStreamM2S 4 ()))
  -- ^ Input packet stream from the source
  -> Signal dom PacketStreamS2M
  -- ^ Input backpressure from the sink
  -> ( Signal dom PacketStreamS2M
     , Signal dom (Maybe (PacketStreamM2S 1 ()))
     )
  -- ^ Output backpressure to the source
  --   Output packet stream to the sink
downConverter fwdInS bwdInS = mealyB go s0 (fwdInS, bwdInS)
  where
    s0 = DownConverterState
      { _dcBuf = errorX "downConverter: undefined initial value"
      , _dcSize = 0
      , _dcLastVec = False
      , _dcAborted = False
      }
    go
      :: DownConverterState 4
      -> (Maybe (PacketStreamM2S 4 ()), PacketStreamS2M)
      -> (DownConverterState 4, (PacketStreamS2M, Maybe (PacketStreamM2S 1 ())))
    go st@(DownConverterState {..}) (fwdIn, PacketStreamS2M inReady) = (st', (bwdOut, fwdOut))
      where
        -- Compute next buffer and its size. If a byte was just acknowledged,
        -- a byte is removed. Otherwise, it is left unchanged.
        (_buf', _size') = if inReady && _dcSize > 0
          then (_dcBuf <<+ errorX "downConverter: undefined value out of bounds", _dcSize - 1)
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
sinkReadyInp = fmap PacketStreamS2M ([False, False, False, True, True, True, True] L.++ (L.repeat True))

clk :: Clock System
clk = systemClockGen

rst :: Reset System
rst = systemResetGen

en :: Enable dom
en = enableGen

payloadOut :: Signal System (Maybe (PacketStreamM2S 1 ()))
sinkReadyOut :: Signal System PacketStreamS2M
downConverterClk = exposeClockResetEnable downConverter clk rst en

(sinkReadyOut, payloadOut) = downConverterClk (fromList payloadInp) (fromList sinkReadyInp)

sampleOut = sampleN 26 $ bundle (payloadOut, sinkReadyOut)
