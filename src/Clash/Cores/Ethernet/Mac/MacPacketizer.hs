{-|
Module      : Clash.Cores.Ethernet.Mac.MacPacketizer
Description : Specialized packetizer for ethernet header.
-}
module Clash.Cores.Ethernet.Mac.MacPacketizer
  (macPacketizerC) where

import Clash.Prelude

import Protocols

import Clash.Cores.Ethernet.Mac.EthernetTypes
import Protocols.Extra.PacketStream
import Protocols.Extra.PacketStream.Packetizer


-- | Prepends an `EthernetHeader` in the metadata to the packet stream, for each packet.
macPacketizerC
  :: forall (dom :: Domain)
            (dataWidth :: Nat) .
  ( HiddenClockResetEnable dom
  , KnownDomain dom
  , 1 <= dataWidth
  , KnownNat dataWidth)
  => Circuit (PacketStream dom dataWidth EthernetHeader) (PacketStream dom dataWidth ())
macPacketizerC = packetizerC (const ()) id
