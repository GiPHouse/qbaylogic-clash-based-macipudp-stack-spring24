{-|
Module      : Clash.Cores.Ethernet.Arp
Description : Provides a functional ARP stack.
-}

{-# language FlexibleContexts #-}
{-# OPTIONS -fplugin=Protocols.Plugin #-}

module Clash.Cores.Ethernet.Arp where

import Clash.Prelude

import Protocols
import Protocols.Df qualified as Df
import Protocols.Extra.PacketStream

import Clash.Cores.Ethernet.Arp.ArpTable
import Clash.Cores.Ethernet.Arp.ArpTypes
import Clash.Cores.Ethernet.EthernetTypes
import Clash.Cores.Ethernet.IP.IPv4Types


arpReceiveC :: Circuit (PacketStream dom dataWidth EthernetHeader) (Df dom ArpEntry, Df dom ArpLite)
arpReceiveC = undefined

arpManagerC :: SNat maxWaitSeconds -> Circuit (ArpLookup dom) (ArpLookup dom, Df dom ArpLite)
arpManagerC = undefined

-- | TODO
arpC
  :: forall
     (dom :: Domain)
     (maxAgeSeconds :: Nat)
     (maxWaitSeconds :: Nat)
     (dataWidth :: Nat)
   . HiddenClockResetEnable dom
  => KnownNat dataWidth
  => KnownNat (DomainPeriod dom)
  => DomainPeriod dom <= 10^12
  => 1 <= DomainPeriod dom
  => 1 <= maxAgeSeconds
  => 1 <= maxWaitSeconds
  => 1 <= dataWidth
  => SNat maxAgeSeconds
  -- ^ ARP entries will expire after this many seconds
  -> SNat maxWaitSeconds
    -- ^ The maximum amount of seconds we wait for an incoming ARP reply
    --   if the lookup IPv4 address was not found in our ARP table
  -> Signal dom MacAddress
  -- ^ Our MAC address
  -> Signal dom IPv4Address
  -- ^ Our IPv4 address
  -> Circuit (PacketStream dom dataWidth EthernetHeader, ArpLookup dom) (PacketStream dom dataWidth EthernetHeader)
arpC maxAge maxWait ourMacS ourIPv4S = case compareSNat d1 (SNat @(10^12 `Div` DomainPeriod dom)) of 
  SNatLE ->
    circuit $ \(ethernetStream, lookupRequest) -> do
      (entry, arpReplyOut) <- arpReceiveC -< ethernetStream
      (lookupOut, arpRequestOut) <- arpManagerC maxWait -< lookupRequest
      () <- arpTable maxAge -< (lookupOut, entry)
      arpPktOut <- Df.roundrobinCollect Df.Skip -< [arpReplyOut, arpRequestOut]
      arpTransmitter ourMacS ourIPv4S -< arpPktOut
  SNatGT -> errorX "arpC: Absurd, Report this to the Clash compiler team: https://github.com/clash-lang/clash-compiler/issues"
