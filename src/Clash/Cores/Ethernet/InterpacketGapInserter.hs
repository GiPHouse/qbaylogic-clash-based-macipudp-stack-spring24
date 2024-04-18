{-# language FlexibleContexts #-}

{-|
Module      : Clash.Cores.Ethernet.InterpacketGapInserter
Description : Circuit for inserting the interpacket gap between ethernet frames
-}
module Clash.Cores.Ethernet.InterpacketGapInserter
  (interpacketGapInserterC) where

import Data.Maybe

import Clash.Prelude
import Protocols.Internal ( Circuit, fromSignals )

import Clash.Cores.Ethernet.PacketStream


data InterpacketGapInserterState gapSize
  = Insert { cycles :: Index gapSize }
  | Forward
  deriving (Show, Generic, NFDataX)


-- | State transition function of the interpacket gap inserter, in mealy form.
gapInserterT :: forall (gapSize :: Nat) .
  ( 1 <= gapSize
  , KnownNat gapSize)
  => InterpacketGapInserterState gapSize
  -> (Maybe (PacketStreamM2S 1 ()),
      PacketStreamS2M
     )
  -- ^ Input packetstream from DownConverter and input backpressure from PHY tx
  -> (InterpacketGapInserterState gapSize,
       (PacketStreamS2M,
        Maybe (PacketStreamM2S 1 ())
       )
     )
  -- ^ Output packetstream to PHY tx and output backpressure to DownConverter
-- Assert backpressure for `gapSize` clock cycles. During these cycles, the output is Nothing.
gapInserterT Insert { cycles = c } (_, _) = (nextState, (PacketStreamS2M False, Nothing))
  where
    nextState = if c == maxBound then Forward else Insert { cycles = c + 1 }
-- Forward incoming data. Once the last flag is set, we insert the interpacket gap.
gapInserterT Forward (Just inp, inReady) = (nextState, (inReady, Just inp))
  where
    nextState = if isJust (_last inp) then Insert { cycles = 0 } else Forward
gapInserterT s (Nothing, inReady) = (s, (inReady, Nothing))

-- | Inserts the interpacket gap between packets. More specifically,
-- this component asserts backpressure for `gapSize` clock cyles after receiving a frame with _last set.
-- During these cycles, the output of this component is Nothing.
interpacketGapInserterC :: forall (dom :: Domain) (gapSize :: Nat) .
  ( HiddenClockResetEnable dom
  , 1 <= gapSize
  , KnownNat gapSize)
  => SNat gapSize -> Circuit (PacketStream dom 1 ()) (PacketStream dom 1 ())
interpacketGapInserterC _ = fromSignals $ mealyB (gapInserterT @gapSize) s0
  where
    s0 = Forward
