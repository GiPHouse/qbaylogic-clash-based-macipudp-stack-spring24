{-# language FlexibleContexts #-}

module Clash.Cores.Ethernet.InterpacketGapInserter
  (interpacketGapInserterC) where

import Data.Maybe

import Clash.Prelude
import Protocols.Internal ( Circuit, fromSignals )

import Clash.Cores.Ethernet.PacketStream


data InterpacketGapInserterState
  = Insert { cycles :: Unsigned 4 }
  | Forward
  deriving (Show, Generic, NFDataX)

-- State transition function of the interpacket gap inserter, in mealy form.
gapInserterT ::
  InterpacketGapInserterState ->
  (Maybe (PacketStreamM2S 1 ()),
   PacketStreamS2M
  )
  -- ^ Input packetstream from DownConverter
  -- ^ Input backpressure from PHY tx
  ->
  (InterpacketGapInserterState,
    (PacketStreamS2M,
     Maybe (PacketStreamM2S 1 ())
    )
  )
  -- ^ Output packetstream to PHY tx
  -- ^ Output backpressure to DownConverter
-- Assert backpressure for 12 clock cycles. During these cycles, the output is Nothing.
gapInserterT Insert { cycles = c } (_, _) = (nextState, (outReady, out))
  where
    nextState = if c < 12 then Insert { cycles = c + 1 } else Forward
    out = Nothing
    outReady = PacketStreamS2M False
-- Forward incoming data. Once the last flag is set, we insert the interpacket gap.
gapInserterT Forward (Just inp, inReady) = (nextState, (outReady, out))
  where
    nextState = if isJust (_last inp) then Insert { cycles = 1 } else Forward
    out = Just inp
    outReady = inReady
gapInserterT s (Nothing, inReady) = (nextState, (outReady, out))
  where
    nextState = s
    out = Nothing
    outReady = inReady

-- | Inserts the interpacket gap between packets. More specifically,
-- this component asserts backpressure for 12 clock cyles after receiving a frame with _last set.
-- During these cycles, the output of this component is Nothing.
interpacketGapInserterC :: forall (dom :: Domain) .
  HiddenClockResetEnable dom
  => Circuit (PacketStream dom 1 ()) (PacketStream dom 1 ())
interpacketGapInserterC = fromSignals $ mealyB gapInserterT s0
  where
    s0 = Forward
