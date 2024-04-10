module Clash.Cores.Ethernet.UpConverter
  ( padPacketC
  ) where

import Clash.Prelude
import Protocols
import Clash.Cores.Ethernet.PacketStream

padPacketC
  :: forall (dataWidth :: Nat) (dom :: Domain).
  HiddenClockResetEnable dom
  => 1 <= dataWidth
  => KnownNat dataWidth
  => Circuit (PacketStream dom dataWidth ()) (PacketStream dom dataWidth ())
padPacketC = undefined
