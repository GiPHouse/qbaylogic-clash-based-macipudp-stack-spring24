{-|
Module      : Clash.Cores.Ethernet.Arp.ArpTransmitter
Description : Provides a component that attempts to fetch the MAC address corresponding to an IPv4 address.
-}
module Clash.Cores.Ethernet.Arp.ArpTransmitter
  (arpTransmitter) where

import Clash.Prelude

import Clash.Cores.Ethernet.Mac.EthernetTypes
import Protocols.Extra.PacketStream

import Protocols
import Protocols.Df qualified as Df

import Clash.Cores.Ethernet.Arp.ArpTypes
import Clash.Cores.Ethernet.IP.IPv4Types
import Protocols.Extra.PacketStream.PacketizeFromDf


-- | Transmits ARP packets upon request.
arpTransmitter
  :: forall (dom :: Domain)
            (dataWidth :: Nat)
   . HiddenClockResetEnable dom
  => 1 <= dataWidth
  => KnownNat dataWidth
  => Signal dom MacAddress
  -- ^ Our MAC address
  -> Signal dom IPv4Address
  -- ^ Our IPv4 address
  -> Circuit (Df dom ArpLite) (PacketStream dom dataWidth EthernetHeader)
arpTransmitter ourMacS ourIPv4S = fromSignals bundleWithSrc |> packetizeFromDfC toEthernetHdr constructArpPkt
  where
    bundleWithSrc (fwdIn, bwdIn) = (bwdIn, go <$> bundle (ourMacS, ourIPv4S, fwdIn))
    go (ourMac, ourIPv4, maybeArpLite) = maybeArpLite >>= \arpLite -> Df.Data (ourMac, ourIPv4, arpLite)

    toEthernetHdr (ourMac, _, arpLite)
      = EthernetHeader {
        _macDst = _targetMac arpLite,
        _macSrc = ourMac,
        _etherType = arpEtherType
      }

    constructArpPkt (ourMac, ourIPv4, arpLite)
      = newArpPacket ourMac ourIPv4 (_targetMac arpLite) (_targetIPv4 arpLite) (_isRequest arpLite)
