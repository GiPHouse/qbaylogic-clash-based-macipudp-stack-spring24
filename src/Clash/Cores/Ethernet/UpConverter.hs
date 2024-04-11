{-# language RecordWildCards #-}
module Clash.Cores.Ethernet.UpConverter
  ( upConverterC
  ) where

import Clash.Prelude
import Data.Maybe ( isJust, isNothing )

import Clash.Cores.Ethernet.PacketStream

import Protocols ( Circuit(..), fromSignals, (|>) )



-- | Upconverter state, consisting of at most p (BitVector 8)'s and a vector indicating which bytes are valid
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

-- | Maybe put this in a utility module?
toMaybe :: Bool -> a -> Maybe a
toMaybe True x = Just x
toMaybe False _ = Nothing

-- | Takes a state for the UpConverter and extracts the output for the UpConverter
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

-- | Up converter circuit. Converts a packet stream of single bytes to a packet
-- stream of arbitrary data widths. A packet has the abort bit set if any of the
-- input packets has the abort bit.
-- Does not stall.
upConverterC
  :: forall (dataWidth :: Nat) (dom :: Domain).
  HiddenClockResetEnable dom
  => 1 <= dataWidth
  => KnownNat dataWidth
  => Circuit (PacketStream dom 1 ()) (PacketStream dom dataWidth ())
upConverterC = forceResetSanity |> fromSignals upConverter
