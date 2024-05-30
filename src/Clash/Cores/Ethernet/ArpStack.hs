{-|
Module      : Clash.Cores.Ethernet.ArpStack
Description : Fully functional ARP stack
-}

{-# language FlexibleContexts #-}

module Clash.Cores.Ethernet.ArpStack
( arpStackC
) where

import Clash.Prelude

import Clash.Cores.Arp
import Clash.Cores.Arp.ArpTypes
import Clash.Cores.Ethernet.EthernetTypes
import Clash.Cores.Ethernet.RxStack
import Clash.Cores.Ethernet.TxStack

import Clash.Cores.Crc
import Clash.Cores.Crc.Catalog
import Clash.Cores.Ethernet.PacketArbiter
import Clash.Cores.Ethernet.PacketDispatcher
import Clash.Cores.Ethernet.PacketStream
import Clash.Cores.IP.IPv4Types
import Protocols


-- | TODO replace this by the IPv4 -> Ethernet stream transformer
constArpLookup :: Circuit () (ArpLookup dom)
constArpLookup = Circuit $ \((), _bwdIn) -> ((), fwdOut)
  where
    fwdOut = pure (Just (IPv4Address (192 :> 168 :> 1 :> 254 :> Nil)))

-- | Fully functional ARP stack.
arpStackC
  :: forall
       (dom :: Domain)
       (domEthRx :: Domain)
       (domEthTx :: Domain)
   . KnownDomain dom
  => KnownDomain domEthRx
  => KnownDomain domEthTx
  => KnownNat (DomainPeriod dom)
  => DomainPeriod dom <= 5 * 10^11
  => 1 <= DomainPeriod dom
  => HardwareCrc Crc32_ethernet 8 4
  => HiddenClockResetEnable dom
  => Clock domEthRx
  -> Reset domEthRx
  -> Enable domEthRx
  -> Clock domEthTx
  -> Reset domEthTx
  -> Enable domEthTx
  -> Signal dom MacAddress
  -> Signal dom IPv4Address
  -> Circuit (PacketStream domEthRx 1 ()) (PacketStream domEthTx 1 ())
arpStackC rxClk rxRst rxEn txClk txRst txEn ourMacS ourIPv4S =
  circuit $ \stream -> do
    ethStream <- rxStack @4 rxClk rxRst rxEn ourMacS -< stream
    [arpStream] <- packetDispatcherC (singleton $ \hdr -> _etherType hdr == arpEtherType) -< ethStream
    lookupIn <- constArpLookup -< ()
    arpOtp <- arpC d10 d5 ourMacS ourIPv4S -< (arpStream, lookupIn)
    ethOtp <- packetArbiterC RoundRobin -< [arpOtp]
    txStack txClk txRst txEn -< ethOtp
