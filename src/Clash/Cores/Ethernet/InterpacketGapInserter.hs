{-# language FlexibleContexts #-}

module Clash.Cores.Ethernet.InterpacketGapInserter (interpacketGapInserterC) where

import Data.Maybe

import Clash.Prelude
import Protocols.Internal ( Circuit, fromSignals )

import Clash.Cores.Ethernet.PacketStream


data InterpacketGapInserterState
  = Idle
  | Insert {cycles :: Unsigned 4}
  | Forward
  deriving (Show, Generic, NFDataX)

gapSize :: Unsigned 4
gapSize = 12

-- State transition function of the interpacket gap inserter
inserterStateT ::
  InterpacketGapInserterState ->
  ( Maybe (PacketStreamM2S 1 ()),
    PacketStreamS2M
  )
  -- | Input packetstream from DownConverter
  -- ^ Input backpressure from PHY tx
  ->
  ( InterpacketGapInserterState,
    ( PacketStreamS2M,
      Maybe (PacketStreamM2S 1 ())
    )
  )
  -- | Output packetstream to PHY tx
  -- ^ Output backpressure to DownConverter
inserterStateT Idle (Just _, inReady) = (nextState, (outReady, out))
  where
    nextState = Insert { cycles = 1 }
    out = Nothing
    outReady = inReady
    -- ^ Wait for an incoming packet. Once an incoming packet arrives, we insert the interpacket gap.
inserterStateT Insert { cycles = c } (_, _) = (nextState, (outReady, out))
  where
    nextState = if c < gapSize then Insert { cycles = c+1 } else Forward
    out = Nothing
    outReady = PacketStreamS2M False
    -- ^ Assert backpressure for `gapSize` clock cycles. During this, our output is Nothing.
inserterStateT Forward (Just inp, inReady) = (nextState, (outReady, out))
  where
    nextState = if isJust (_last inp) then Idle else Forward
    out = Just inp
    outReady = inReady
    -- ^ Forward the incoming packet. Once the last flag is set, we go back to Idle.
inserterStateT s (Nothing, inReady) = (nextState, (outReady, out))
  where
    nextState = s
    out = Nothing
    outReady = inReady

interpacketGapInserterC :: forall (dom :: Domain) .
  HiddenClockResetEnable dom =>
  Circuit (PacketStream dom 1 ()) (PacketStream dom 1 ())
interpacketGapInserterC = fromSignals $ mealyB inserterStateT s0
    where s0 = Idle
