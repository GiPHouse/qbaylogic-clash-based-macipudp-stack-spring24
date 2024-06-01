{-# language FlexibleContexts #-}
{-# language RecordWildCards #-}

{-|
Module      : Clash.Cores.Ethernet.Examples.EchoStack
Description : Simple Ethernet echo stack.
-}
module Clash.Cores.Ethernet.Examples.EchoStack
  ( ipEchoStackC
  , fullStackC
  , arpIcmpUdpStackC
  ) where

import Data.Bifunctor qualified as B

-- import prelude
import Clash.Prelude

-- import ethernet
import Clash.Cores.Ethernet.Arp
import Clash.Cores.Ethernet.Examples.RxStacks
import Clash.Cores.Ethernet.Examples.TxStacks
import Clash.Cores.Ethernet.IP.IPPacketizers
import Clash.Cores.Ethernet.Mac.EthernetTypes ( EthernetHeader(..), MacAddress(..) )

import Clash.Cores.Ethernet.IP.EthernetStream
import Clash.Cores.Ethernet.IP.IPv4Types

-- import protocols
import Protocols
import Protocols.Extra.PacketStream
import Protocols.Extra.PacketStream.PacketBuffer ( packetBufferC )
import Protocols.Extra.PacketStream.Routing

import Clash.Cores.Crc ( HardwareCrc )
import Clash.Cores.Crc.Catalog ( Crc32_ethernet )

import Clash.Cores.Ethernet.Icmp ( icmpEchoResponderC )
import Clash.Cores.Ethernet.Udp


-- | Processes IP packets and echoes them back
ipEchoStackC
  :: forall
       (dom :: Domain)
       (domEthRx :: Domain)
       (domEthTx :: Domain)
   . KnownDomain dom
  => KnownDomain domEthRx
  => KnownDomain domEthTx
  => HardwareCrc Crc32_ethernet 8 4
  => HiddenClockResetEnable dom
  => Clock domEthRx
  -> Reset domEthRx
  -> Enable domEthRx
  -> Clock domEthTx
  -> Reset domEthTx
  -> Enable domEthTx
  -> Signal dom MacAddress
  -> Signal dom (IPv4Address, IPv4Address)
  -> Circuit (PacketStream domEthRx 1 ()) (PacketStream domEthTx 1 ())
ipEchoStackC rxClk rxRst rxEn txClk txRst txEn mac ip = ckt
  where
    swapIp hdr@IPv4HeaderLite {..} = hdr { _ipv4lSource = _ipv4lDestination, _ipv4lDestination = _ipv4lSource}
    ckt = ipRxStack @4 rxClk rxRst rxEn mac ip
            |> packetBufferC d10 d4
            |> mapMeta swapIp
            |> ipTxStack @4 txClk txRst txEn mac
fullStackC
  :: forall
       (dom :: Domain)
       (domEthRx :: Domain)
       (domEthTx :: Domain)
   . KnownDomain dom
  => KnownDomain domEthRx
  => KnownDomain domEthTx
  => HardwareCrc Crc32_ethernet 8 4
  => 1 <= DomainPeriod dom
  => DomainPeriod dom <= 5 * 10^11
  => KnownNat (DomainPeriod dom)
  => HiddenClockResetEnable dom
  => Clock domEthRx
  -> Reset domEthRx
  -> Enable domEthRx
  -> Clock domEthTx
  -> Reset domEthTx
  -> Enable domEthTx
  -> Signal dom MacAddress
  -> Signal dom (IPv4Address, IPv4Address)
  -> Circuit (PacketStream domEthRx 1 ()) (PacketStream domEthTx 1 ())
fullStackC rxClk rxRst rxEn txClk txRst txEn mac ip =
  macRxStack @4 rxClk rxRst rxEn mac
  |> arpIcmpUdpStackC mac ip (mapMeta $ B.second swapPorts)
  |> macTxStack txClk txRst txEn
  where
    swapPorts hdr@UdpHeaderLite{..} = hdr
                                        { _udplSrcPort = _udplDstPort
                                        , _udplDstPort = _udplSrcPort
                                        }

arpIcmpUdpStackC
  :: forall (dataWidth :: Nat) (dom :: Domain)
   . HiddenClockResetEnable dom
  => KnownNat dataWidth
  => 1 <= dataWidth
  => 1 <= DomainPeriod dom
  => DomainPeriod dom <= 5 * 10^11
  => KnownNat (DomainPeriod dom)
  => Signal dom MacAddress
  -- ^ My MAC Address
  -> Signal dom (IPv4Address, IPv4Address)
  -- ^ My IP address and the subnet
  -> Circuit (PacketStream dom dataWidth (IPv4Address, UdpHeaderLite)) (PacketStream dom dataWidth (IPv4Address, UdpHeaderLite))
  -- ^ UDP handler circuit
  -> Circuit (PacketStream dom dataWidth EthernetHeader) (PacketStream dom dataWidth EthernetHeader)
arpIcmpUdpStackC macAddressS ipS udpCkt = circuit $ \ethIn -> do
  [arpEthIn, ipEthIn] <- packetDispatcherC (routeBy _etherType $ 0x0806 :> 0x0800 :> Nil) -< ethIn
  ipTx <- ipLitePacketizerC <| packetBufferC d10 d4 <| icmpUdpStack <| packetBufferC d10 d4 <| filterMetaS (isForMyIp <$> ipS) <| ipDepacketizerLiteC -< ipEthIn
  (ipEthOut, arpLookup) <- toEthernetStreamC macAddressS -< ipTx
  arpEthOut <- arpC d10 d5 macAddressS (fst <$> ipS) -< (arpEthIn, arpLookup)
  packetArbiterC RoundRobin -< [arpEthOut, ipEthOut]

  where
    icmpUdpStack = circuit $ \ipIn -> do
      [icmpIn, udpIn] <- packetDispatcherC (routeBy _ipv4lProtocol $ 0x0001 :> 0x0011 :> Nil) -< ipIn
      icmpOut <- icmpEchoResponderC @dom @dataWidth (fst <$> ipS) -< icmpIn
      udpInParsed <- udpDepacketizerC -< udpIn
      udpOutParsed <- udpPacketizerC (fst <$> ipS) <| udpCkt -< udpInParsed
      packetArbiterC RoundRobin -< [icmpOut, udpOutParsed]
    isForMyIp (ip, subnet) (_ipv4lDestination -> to) = to == ip || to == ipv4Broadcast ip subnet
