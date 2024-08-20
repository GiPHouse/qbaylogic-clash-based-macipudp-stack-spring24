{-# language FlexibleContexts #-}
{-# language RecordWildCards #-}

module Clash.Cores.Ethernet.Examples.UdpCoreStack where

-- import prelude
import Clash.Prelude

import Data.Proxy

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
import qualified Protocols.DfConv as DfConv
import Protocols.Extra.PacketStream
import Protocols.Extra.PacketStream.PacketBuffer ( packetBufferC )
import Protocols.Extra.PacketStream.Routing

import Clash.Cores.Crc ( HardwareCrc )
import Clash.Cores.Crc.Catalog ( Crc32_ethernet )

import Clash.Cores.Ethernet.Icmp ( icmpEchoResponderC )
import Clash.Cores.Ethernet.Udp

-- | Full stack from ethernet to ethernet.
fullStackC
  :: forall
       (dataWidth :: Nat)
       (dom :: Domain)
       (domEthRx :: Domain)
       (domEthTx :: Domain)
   . KnownDomain dom
  => KnownDomain domEthRx
  => KnownDomain domEthTx
  => HardwareCrc Crc32_ethernet 8 dataWidth
  => KnownNat dataWidth
  => 1 <= dataWidth
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
  -- ^ My mac address
  -> Signal dom (IPv4Address, IPv4Address)
  -- ^ Tuple of my IP and subnet mask
  -- Input tuple is UDP packets to transmit including destination IP and input from Phy
  -- Output tuple are the incoming UDP packets including the destination IP and output to Phy
  -> Circuit
       (PacketStream dom dataWidth (IPv4Address, UdpHeaderLite), PacketStream domEthRx 1 ())
       (PacketStream dom dataWidth (IPv4Address, UdpHeaderLite), PacketStream domEthTx 1 ())
fullStackC rxClk rxRst rxEn txClk txRst txEn macS ipS = circuit $ \(udpOut, phyIn) -> do
  ethIn <- macRxStack @dataWidth rxClk rxRst rxEn macS -< phyIn
  udpOutBuffered <- packetBufferC d10 d4 -< udpOut
  (udpIn, ethOut) <- arpIcmpUdpStackC macS ipS -< (udpOutBuffered, ethIn)
  udpInBuffered <-packetBufferC d10 d4 -< udpIn
  phyOut <- macTxStack txClk txRst txEn -< ethOut
  idC -< (udpInBuffered, phyOut)

regFwd
  :: HiddenClockResetEnable dom
  => KnownNat n
  => NFDataX meta
  => Circuit (PacketStream dom n meta) (PacketStream dom n meta)
regFwd = DfConv.registerFwd Proxy Proxy

regBwd
  :: HiddenClockResetEnable dom
  => KnownNat n
  => NFDataX meta
  => Circuit (PacketStream dom n meta) (PacketStream dom n meta)
regBwd = DfConv.registerBwd Proxy Proxy

regBidir
  :: HiddenClockResetEnable dom
  => KnownNat n
  => NFDataX meta
  => Circuit (PacketStream dom n meta) (PacketStream dom n meta)
regBidir = regBwd |> regFwd

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
  -> Circuit
      (PacketStream dom dataWidth (IPv4Address, UdpHeaderLite), PacketStream dom dataWidth EthernetHeader)
      (PacketStream dom dataWidth (IPv4Address, UdpHeaderLite), PacketStream dom dataWidth EthernetHeader)
arpIcmpUdpStackC macAddressS ipS = circuit $ \(udpOut, ethIn) -> do
  [arpEthIn, ipEthIn] <- packetDispatcherC (routeBy _etherType $ 0x0806 :> 0x0800 :> Nil) -< ethIn
  ipIn <- filterMetaS (isForMyIp <$> ipS) <| regBidir <| ipDepacketizerLiteC <| regBidir -< ipEthIn
  (udpIn, ipOut) <- icmpUdpStackC ipS -< (udpOut, ipIn)
  (ipEthOut, arpLookup) <- toEthernetStreamC macAddressS <| regBidir <| ipLitePacketizerC <| regBidir -< ipOut
  arpEthOut <- arpC d300 d2 d6 macAddressS (fst <$> ipS) -< (arpEthIn, arpLookup)
  ethOut <- packetArbiterC RoundRobin -< [arpEthOut, ipEthOut]
  idC -< (udpIn, ethOut)
  where
    isForMyIp (ip, subnet) (_ipv4lDestination -> to) = to == ip || to == ipv4Broadcast ip subnet

icmpUdpStackC
  :: forall (dataWidth :: Nat) (dom :: Domain)
   . HiddenClockResetEnable dom
  => KnownNat dataWidth
  => 1 <= dataWidth
  => Signal dom (IPv4Address, IPv4Address)
  -- ^ My IP address and the subnet
  -> Circuit
      (PacketStream dom dataWidth (IPv4Address, UdpHeaderLite), PacketStream dom dataWidth IPv4HeaderLite)
      (PacketStream dom dataWidth (IPv4Address, UdpHeaderLite), PacketStream dom dataWidth IPv4HeaderLite)
icmpUdpStackC ipS = circuit $ \(udpOut, ipIn) -> do
  [icmpIn, udpIn] <- packetDispatcherC (routeBy _ipv4lProtocol $ 0x0001 :> 0x0011 :> Nil) -< ipIn
  icmpOut <- icmpEchoResponderC @dom @dataWidth (fst <$> ipS) -< icmpIn
  udpInParsed <- udpDepacketizerC -< udpIn
  udpOutParsed <- udpPacketizerC (fst <$> ipS) -< udpOut
  ipOut <- packetArbiterC RoundRobin -< [icmpOut, udpOutParsed]
  idC -< (udpInParsed, ipOut)
