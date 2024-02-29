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
    _dc_buf  :: Vec dataWidth (BitVector 8),
    -- ^Buffer
    _size :: Index (dataWidth + 1),
    -- ^Number of valid bytes in _dc_buf
    _lastVec :: Bool,
    -- ^True if last byte of _dc_buf was marked as last byte by incoming stream
    _dc_aborted :: Bool
    -- ^If True, outgoing bytes should be marked as aborted until _dc_buf is replaced
  }
  deriving (Generic, NFDataX)

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
      { _dc_buf = undefined
      , _size = 0
      , _lastVec = False
      , _dc_aborted = False
      }
    go
      :: DownConverterState 4
      -> (Maybe (PacketStreamM2S 4 ()), PacketStreamS2M)
      -> (DownConverterState 4, (PacketStreamS2M, Maybe (PacketStreamM2S 1 ())))
    go st@(DownConverterState {..}) (fwdIn, PacketStreamS2M inReady) = (st', (bwdOut, fwdOut))
      where
        -- Compute next buffer and its size. If a byte was just acknowledged,
        -- a byte is removed. Otherwise, it is left unchanged.
        (_buf', _size') = if inReady && _size > 0
          then (fst $ shiftOutFrom0 d1 _dc_buf, _size - 1)
          else (_dc_buf, _size)
        -- If our next buffer will be empty, we are ready to receive new data,
        -- and if there is valid data already, put it in a fresh state.
        -- Otherwise, keep the current state with the new buffer and size.
        outReady = _size' == 0
        st' = case (outReady, fwdIn) of
          (True, Just packetStream) -> fromPacketStreamM2S packetStream
          _                         -> st { _dc_buf = _buf', _size = _size' }
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
