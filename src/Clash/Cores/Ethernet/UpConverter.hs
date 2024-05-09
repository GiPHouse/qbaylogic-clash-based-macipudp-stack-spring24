{-# language RecordWildCards #-}

{-|
Module      : Clash.Cores.Ethernet.UpConverter
Description : Provides an up converter, for changing the data width of packet streams
-}
module Clash.Cores.Ethernet.UpConverter
  ( upConverterC
  ) where

import Clash.Cores.Ethernet.PacketStream
import Clash.Cores.Ethernet.Util
import Clash.Prelude
import Data.Maybe ( isJust, isNothing )
import Protocols ( Circuit(..), fromSignals, (|>) )

-- | Upconverter state, consisting of at most p `BitVector 8`s and a vector indicating which bytes are valid
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

toPacketStream :: UpConverterState dataWidth -> Maybe (PacketStreamM2S dataWidth ())
toPacketStream UpConverterState{..} = toMaybe _ucFlush (PacketStreamM2S _ucBuf _ucLastIdx () _ucAborted)

nextState
  :: KnownNat dataWidth
  => UpConverterState dataWidth
  -> Maybe (PacketStreamM2S 1 ())
  -> PacketStreamS2M
  -> UpConverterState dataWidth
nextState st@(UpConverterState {..}) Nothing (PacketStreamS2M inReady)
  = nextSt
    where
      outReady = not _ucFlush || inReady
      -- If we can accept data we can always set _ucFlush to false,
      -- since we only change state if we can transmit and receive data
      nextStRaw = st
                    { _ucFlush = False
                    , _ucAborted = isNothing _ucLastIdx && _ucAborted
                    , _ucLastIdx = Nothing
                    }
      nextSt = if outReady then nextStRaw else st
nextState st@(UpConverterState {..}) (Just PacketStreamM2S{..}) (PacketStreamS2M inReady)
  = nextSt
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
    go st@(UpConverterState {..}) (fwdIn, bwdIn)
      = (nextState st fwdIn bwdIn, (PacketStreamS2M outReady, toPacketStream st))
        where
          outReady = not _ucFlush || (_ready bwdIn)

-- | Converts packet streams of single bytes to packet streams of a higher data widths.
-- Has one cycle of latency, but optimal throughput.
upConverterC
  :: forall (dataWidth :: Nat) (dom :: Domain).
  HiddenClockResetEnable dom
  => 1 <= dataWidth
  => KnownNat dataWidth
  => Circuit (PacketStream dom 1 ()) (PacketStream dom dataWidth ())
upConverterC = forceResetSanity |> fromSignals upConverter
