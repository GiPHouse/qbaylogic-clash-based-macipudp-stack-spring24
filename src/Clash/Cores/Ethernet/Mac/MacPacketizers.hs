{-|
Module      : Clash.Cores.Ethernet.Mac.MacPacketizers
Description : Specialized packetizers for ethernet headers.
-}
module Clash.Cores.Ethernet.Mac.MacPacketizers
  ( macPacketizerC
  , macDepacketizerC
  ) where

import Clash.Prelude

import Protocols
import Protocols.Extra.PacketStream
import Protocols.Extra.PacketStream.Packetizers

import Clash.Cores.Ethernet.Mac.EthernetTypes


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

-- | Parses the first 14 bytes of the incoming PacketStream into an `EthernetHeader`.
macDepacketizerC :: forall (dom :: Domain) (dataWidth :: Nat).
  ( KnownDomain dom
  , HiddenClockResetEnable dom
  , KnownNat dataWidth
  , 1 <= dataWidth
  )
  => Circuit (PacketStream dom dataWidth ()) (PacketStream dom dataWidth EthernetHeader)
macDepacketizerC = depacketizerC const
