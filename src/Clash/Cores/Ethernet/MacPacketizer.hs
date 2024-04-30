{-|
Module      : Clash.Cores.Ethernet.MacPacketizer
Description : Specialized packetizer for ethernet header
-}
module Clash.Cores.Ethernet.MacPacketizer
  (macPacketizerC) where

import Clash.Prelude

import Protocols

import Clash.Cores.Ethernet.EthernetTypes
import Clash.Cores.Ethernet.Packetizer
import Clash.Cores.Ethernet.PacketStream


-- | Prepends an @EthernetHeader@ in the metadata to the packet stream, for each packet.
macPacketizerC
  :: forall (dom :: Domain)
            (dataWidth :: Nat) .
  ( HiddenClockResetEnable dom
  , KnownDomain dom
  , 1 <= dataWidth
  , KnownNat dataWidth)
  => Circuit (PacketStream dom dataWidth EthernetHeader) (PacketStream dom dataWidth ())
macPacketizerC = packetizerC (const ()) id
