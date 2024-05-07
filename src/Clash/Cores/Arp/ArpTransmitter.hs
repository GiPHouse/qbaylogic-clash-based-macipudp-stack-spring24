{-|
Module      : Clash.Cores.Arp.ArpTransmitter
Description : Provides a component that attempts to fetch the MAC address corresponding to an IP address.
-}
module Clash.Cores.Arp.ArpTransmitter
  (arpTransmitter) where

import Clash.Prelude

import Clash.Cores.Ethernet.EthernetTypes
import Clash.Cores.Ethernet.PacketStream

import Protocols
import Protocols.Df hiding ( const, pure )

import Clash.Cores.Arp.ArpTypes
import Clash.Cores.Ethernet.PacketizeFromDf
import Clash.Cores.IP.IPv4Types


-- | Upon receiving an IP address, this component broadcasts an ARP request
--   to fetch the MAC address corresponding to this IP address.
arpTransmitter
  :: forall (dom :: Domain)
            (dataWidth :: Nat) .
  ( HiddenClockResetEnable dom
  , 1 <= dataWidth
  , KnownNat dataWidth)
  => Signal dom MacAddress
  -- ^ My MAC address
  -> Signal dom IPAddress
  -- ^ My IP address
  -> Circuit (Df dom IPAddress) (PacketStream dom dataWidth EthernetHeader)
arpTransmitter shaS spaS = fromSignals arpBundle |> packetizeFromDfC toEthernet (uncurry $ uncurry newArpRequest)
  where
    arpBundle (fwdIn, bwdIn) = (bwdIn, fmap go (bundle (bundle (shaS, spaS), fwdIn)))
    go ((sha, spa), maybeTha) = case maybeTha of
      NoData -> NoData
      Data tha -> Data ((sha, spa), tha)

    toEthernet ((sha, _), _)
      = EthernetHeader {
          _macDst = broadcastMac,
          _macSrc = sha,
          _etherType = arpEtherType
        }
