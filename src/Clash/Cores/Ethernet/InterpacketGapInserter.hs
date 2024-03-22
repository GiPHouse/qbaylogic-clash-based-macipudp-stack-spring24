{-# language FlexibleContexts #-}

module Clash.Cores.Ethernet.InterpacketGapInserter
  (interpacketGapInserterC) where

import Data.Maybe

import Clash.Prelude
import Protocols.Internal ( Circuit, fromSignals )

import Clash.Cores.Ethernet.PacketStream


data InterpacketGapInserterState
  = Insert { cycles :: Index 12 }
  | Forward
  deriving (Show, Generic, NFDataX)

-- | State transition function of the interpacket gap inserter, in mealy form.
gapInserterT ::
  InterpacketGapInserterState ->
  (Maybe (PacketStreamM2S 1 ()),
   PacketStreamS2M
  )
  -- ^ Input packetstream from DownConverter and input backpressure from PHY tx
  ->
  (InterpacketGapInserterState,
    (PacketStreamS2M,
     Maybe (PacketStreamM2S 1 ())
    )
  )
  -- ^ Output packetstream to PHY tx and output backpressure to DownConverter
-- Assert backpressure for 12 clock cycles. During these cycles, the output is Nothing.
gapInserterT Insert { cycles = c } (_, _) = (nextState, (PacketStreamS2M False, Nothing))
  where
    nextState = if c == 11 then Forward else Insert { cycles = c + 1 }
-- Forward incoming data. Once the last flag is set, we insert the interpacket gap.
gapInserterT Forward (Just inp, inReady) = (nextState, (inReady, Just inp))
  where
    nextState = if isJust (_last inp) then Insert { cycles = 0 } else Forward
gapInserterT s (Nothing, inReady) = (s, (inReady, Nothing))

-- | Inserts the interpacket gap between packets. More specifically,
-- this component asserts backpressure for 12 clock cyles after receiving a frame with _last set.
-- During these cycles, the output of this component is Nothing.
interpacketGapInserterC :: forall (dom :: Domain) .
  HiddenClockResetEnable dom
  => Circuit (PacketStream dom 1 ()) (PacketStream dom 1 ())
interpacketGapInserterC = fromSignals $ mealyB gapInserterT s0
  where
    s0 = Forward
