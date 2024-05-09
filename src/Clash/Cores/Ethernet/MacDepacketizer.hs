{-|
Module      : Clash.Cores.Ethernet.MacDepacketizer
Description : Specialized depacketizer for ethernet header
-}
module Clash.Cores.Ethernet.MacDepacketizer
  ( macDepacketizerC ) where

import Clash.Prelude

import Protocols

import Clash.Cores.Ethernet.Depacketizer
import Clash.Cores.Ethernet.EthernetTypes
import Clash.Cores.Ethernet.PacketStream


-- | Parses the first 14 bytes of the incoming PacketStream into an `EthernetHeader`.
macDepacketizerC :: forall (dom :: Domain) (dataWidth :: Nat).
  ( KnownDomain dom
  , HiddenClockResetEnable dom
  , KnownNat dataWidth
  , 1 <= dataWidth
  )
  => Circuit (PacketStream dom dataWidth ()) (PacketStream dom dataWidth EthernetHeader)
macDepacketizerC = depacketizerC const
