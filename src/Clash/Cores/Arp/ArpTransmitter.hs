{-|
Module      : Clash.Cores.Arp.ArpTransmitter
Description : Provides a component that attempts to fetch the MAC address corresponding to an IPv4 address.
-}
module Clash.Cores.Arp.ArpTransmitter
  (arpTransmitter) where

import Clash.Prelude

import Clash.Cores.Ethernet.EthernetTypes
import Clash.Cores.Ethernet.PacketStream

import Protocols
import Protocols.Df ( Data(..) )

import Clash.Cores.Arp.ArpTypes
import Clash.Cores.Ethernet.PacketizeFromDf
import Clash.Cores.IP.IPv4Types


-- | Upon receiving an IPv4 address, this component broadcasts an ARP request
--   to fetch the MAC address corresponding to this IP address.
arpTransmitter
  :: forall (dom :: Domain)
            (dataWidth :: Nat)
   . HiddenClockResetEnable dom
  => 1 <= dataWidth
  => KnownNat dataWidth
  => Signal dom MacAddress
  -- ^ Our MAC address
  -> Signal dom IPAddress
  -- ^ Our IPv4 address
  -> Circuit (Df dom IPAddress) (PacketStream dom dataWidth EthernetHeader)
arpTransmitter shaS spaS = fromSignals arpBundle |> packetizeFromDfC toEthernetHdr (uncurry $ uncurry newArpRequest)
  where
    arpBundle (fwdIn, bwdIn) = (bwdIn, fmap go (bundle (bundle (shaS, spaS), fwdIn)))
    go ((sha, spa), maybeTha) = maybeTha >>= \tha -> Data ((sha, spa), tha)

    toEthernetHdr ((sha, _), _)
      = EthernetHeader {
          _macDst = broadcastMac,
          _macSrc = sha,
          _etherType = arpEtherType
        }
