module Clash.Cores.Ethernet.MacPacketizer
  (macPacketizerC) where

import Clash.Prelude

import Protocols

import Clash.Cores.Ethernet.EthernetTypes
import Clash.Cores.Ethernet.Packetizer
import Clash.Cores.Ethernet.PacketStream


-- | Prepends the `EthernetHeader` in the `_meta` field of the packet to the PacketStream
macPacketizerC
  :: forall (dom :: Domain)
            (dataWidth :: Nat)
   . HiddenClockResetEnable dom
  => KnownDomain dom
  => 1 <= dataWidth
  => Mod 14 dataWidth <= (dataWidth - 1)
  => KnownNat dataWidth
  => Circuit (PacketStream dom dataWidth EthernetHeader) (PacketStream dom dataWidth ())
macPacketizerC = packetizerC (const ()) id
