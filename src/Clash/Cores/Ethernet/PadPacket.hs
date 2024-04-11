module Clash.Cores.Ethernet.UpConverter
  ( padPacketC
  ) where

import Clash.Cores.Ethernet.PacketStream
import Clash.Prelude
import Protocols

data PadPacketState
  = Forward { cycles :: Index 64}
  | Pad { cycles :: Index 64}
  deriving (Show, Generic, NFDataX)

padPacket
  :: forall (dataWidth :: Nat) (dom :: Domain).
  HiddenClockResetEnable dom
  => 1 <= dataWidth
  => KnownNat dataWidth
  => ( Signal dom (Maybe (PacketStreamM2S dataWidth ()))
     , Signal dom PacketStreamS2M)
  -- ^ Input packet stream from the source
  --   Input backpressure from the sink
  -> ( Signal dom PacketStreamS2M
     , Signal dom (Maybe (PacketStreamM2S dataWidth ()))
     )
  -- ^ Output backpressure to the source
  --   Output packet stream to the sink
padPacket = mealyB go s0
  where
    s0 = Forward 0
    go
      :: PadPacketState
      -> (Maybe (PacketStreamM2S dataWidth ()), PacketStreamS2M)
      -> ( PadPacketState
         , (PacketStreamS2M, Maybe (PacketStreamM2S dataWidth ()))
         )
    go st (fwdIn, bwdIn)
      = undefined

padPacketC
  :: forall (dataWidth :: Nat) (dom :: Domain).
  HiddenClockResetEnable dom
  => 1 <= dataWidth
  => KnownNat dataWidth
  => Circuit (PacketStream dom dataWidth ()) (PacketStream dom dataWidth ())
padPacketC = fromSignals padPacket
