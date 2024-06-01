{-# language FlexibleContexts #-}
{-# language RecordWildCards #-}

{-|
Module      : Clash.Cores.Ethernet.Examples.EchoStack
Description : Simple Ethernet echo stack.
-}
module Clash.Cores.Ethernet.Examples.EchoStack
  ( ipEchoStackC
  , fullStackC
  , arpIcmpStackC
  ) where

-- import prelude
import Clash.Prelude

-- import ethernet
import Clash.Cores.Ethernet.Examples.RxStacks
import Clash.Cores.Ethernet.Examples.TxStacks
import Clash.Cores.Ethernet.Mac.EthernetTypes ( EthernetHeader(..), MacAddress(..) )
import Clash.Cores.Ethernet.Arp
import Clash.Cores.Ethernet.IP.IPPacketizers

import Clash.Cores.Ethernet.IP.IPv4Types
import Clash.Cores.Ethernet.IP.EthernetStream

-- import protocols
import Protocols
import Protocols.Extra.PacketStream
import Protocols.Extra.PacketStream.PacketBuffer ( packetBufferC )
import Protocols.Extra.PacketStream.Routing

import Clash.Cores.Crc ( HardwareCrc )
import Clash.Cores.Crc.Catalog ( Crc32_ethernet )

import Clash.Cores.Ethernet.Icmp ( icmpEchoResponderC )


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
  |> arpIcmpStackC mac ip
  |> macTxStack txClk txRst txEn

arpIcmpStackC
  :: forall (dataWidth :: Nat) (dom :: Domain)
   . HiddenClockResetEnable dom
  => KnownNat dataWidth
  => 1 <= dataWidth
  => 1 <= DomainPeriod dom
  => DomainPeriod dom <= 5 * 10^11
  => KnownNat (DomainPeriod dom)
  => Signal dom MacAddress
  -> Signal dom (IPv4Address, IPv4Address)
  -> Circuit (PacketStream dom dataWidth EthernetHeader) (PacketStream dom dataWidth EthernetHeader)
arpIcmpStackC macAddressS ipS = circuit $ \ethIn -> do
  [arpEthIn, ipEthIn] <- packetDispatcherC (routeBy _etherType $ 0x0806 :> 0x0800 :> Nil) -< ethIn
  ipTx <- ipLitePacketizerC <| icmpStack <| filterMetaS (isForMyIp <$> ipS) <| ipDepacketizerLiteC -< ipEthIn
  (ipEthOut, arpLookup) <- toEthernetStreamC macAddressS -< ipTx
  arpEthOut <- arpC d10 d5 macAddressS (fst <$> ipS) -< (arpEthIn, arpLookup)
  packetArbiterC RoundRobin -< [arpEthOut, ipEthOut]

  where
    icmpStack = filterMeta ((1 ==) . _ipv4lProtocol)
                  |> packetBufferC d10 d4
                  |> icmpEchoResponderC @dom @dataWidth (fst <$> ipS)
    isForMyIp (ip, subnet) (_ipv4lDestination -> to) = to == ip || to == ipv4Broadcast ip subnet