{-# LANGUAGE RecordWildCards #-}
module Clash.Cores.Ethernet.UpConverter
  ( upConverter
  , sampleOut
  ) where

import Clash.Prelude
import Data.Maybe (isJust, isNothing, fromMaybe)

import Clash.Cores.Ethernet.PacketStream

import qualified Data.List as L
import GHC.IO.Handle (noNewlineTranslation)
import Distribution.Simple (laterVersion)

data UpConverterState (dataWidth :: Nat) =
  UpConverterState {
    _uc_buf     :: Vec dataWidth (BitVector 8),
    -- ^ the buffer we are filling
    _idx     :: Index dataWidth,
    -- ^ Where in the buffer we need to write the next element
    _flush   :: Bool,
    -- ^ If this is true the current state can presented as packetstream word
    _uc_aborted :: Bool,
    -- ^ Current packet is aborted
    _lastIdx :: Maybe (Index dataWidth)
    -- ^ If true the current buffer contains the last byte of the current packet
  }
  deriving (Generic, NFDataX)
-- ^ Upconverter state, consisting of at most p (BitVector 8)s and a vector indicating which bytes are valid

-- | Maybe put this in a utility module?
toMaybe :: Bool -> a -> Maybe a
toMaybe True x = Just x
toMaybe False _ = Nothing

toPacketStream :: UpConverterState dataWidth -> Maybe (PacketStreamM2S dataWidth ())
toPacketStream UpConverterState{..} = toMaybe _flush (PacketStreamM2S _uc_buf _lastIdx () _uc_aborted)

upConverter
  :: forall (dataWidth :: Nat) (dom :: Domain).
  HiddenClockResetEnable dom
  => 1 <= dataWidth
  => KnownNat dataWidth
  => Signal dom (Maybe (PacketStreamM2S 1 ()))
  -- ^ Input packet stream from the source
  -> Signal dom PacketStreamS2M
  -- ^ Input backpressure from the sink
  -> ( Signal dom PacketStreamS2M
     , Signal dom (Maybe (PacketStreamM2S dataWidth ()))
     )
  -- ^ Output backpressure to the source
  --   Output packet stream to the sink
upConverter fwdInS bwdInS = mealyB go s0 (fwdInS, bwdInS)
  where
    s0 = UpConverterState (repeat undefined) 0 False False Nothing
    go
      :: UpConverterState dataWidth
      -> (Maybe (PacketStreamM2S 1 ()), PacketStreamS2M)
      -> ( UpConverterState dataWidth
         , (PacketStreamS2M, Maybe (PacketStreamM2S dataWidth ()))
         )
    go st@(UpConverterState {..}) (Nothing, PacketStreamS2M inReady)
      = (nextSt, (PacketStreamS2M outReady, toPacketStream st))
        where
          -- If we can accept data we can always set _flush to false
          -- since we only change state if we can transmit and receive data
          nextStRaw = st
                        { _flush = False
                        , _uc_aborted = isNothing _lastIdx && _uc_aborted
                        , _lastIdx = Nothing
                        }
          outReady = not _flush || inReady
          nextSt = if outReady then nextStRaw else st
    go st@(UpConverterState {..}) (Just (PacketStreamM2S{..}), PacketStreamS2M inReady)
      = (nextSt, (PacketStreamS2M outReady, toPacketStream st))
        where
          inLast = isJust _last
          -- We smear an abort over the entire rest of the packet
          -- So the next abort is set
          --  - If fragment we are potentially flushing was not the last and we were allready aborting
          --  - Or if the incoming fragment is aborted
          nextAbort = (isNothing _lastIdx && _uc_aborted) || _abort
          -- If we are not flushing we can accept data to be stored in _uc_buf
          -- But when we are flushing we can only accept if the current
          -- output fragment is accepted by the sink
          outReady = not _flush || inReady
          bufFull = _idx == maxBound
          nextBuf = replace _idx (head _data) _uc_buf

          nextFlush = inLast || bufFull
          nextIdx = if nextFlush then 0 else _idx + 1

          nextStRaw = UpConverterState
                        { _uc_buf =  nextBuf
                        , _idx = nextIdx
                        , _flush = nextFlush
                        , _uc_aborted = nextAbort
                        , _lastIdx = toMaybe inLast _idx
                        }
          nextSt = if outReady then nextStRaw else st

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
(sinkReadyOut, payloadOut) = upConverterClk (fromList payloadInp) (fromList sinkReadyInp)

sampleOut = sampleN 20 $ bundle (payloadOut, sinkReadyOut)
