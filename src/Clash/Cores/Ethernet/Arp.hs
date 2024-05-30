{-|
Module      : Clash.Cores.Ethernet.Arp
Description : Provides a fully functional ARP stack.
-}

{-# language FlexibleContexts #-}
{-# OPTIONS -fplugin=Protocols.Plugin #-}

module Clash.Cores.Ethernet.Arp where

import Clash.Prelude

import Protocols
import Protocols.Df qualified as Df
import Protocols.Extra.PacketStream

import Clash.Cores.Ethernet.Arp.ArpManager
import Clash.Cores.Ethernet.Arp.ArpTable
import Clash.Cores.Ethernet.Arp.ArpTypes
import Clash.Cores.Ethernet.IP.IPv4Types
import Clash.Cores.Ethernet.Mac.EthernetTypes


-- | A fully functional ARP stack which handles ARP lookups from client circuits.
--   Maintains a single-entry ARP table which the client circuit can query via the
--   `ArpLookup` input. If the client-supplied IPv4 address is not found in the table,
--   it transmits an ARP request for this specific address. The circuit will assert
--   backpressure until either a reply has been received, or a timeout occurs. The
--   maximum number of seconds the stack will wait for a reply to this request is
--   configurable. The timeout (in seconds) of ARP table entries is configurable as well.
--   All timeouts may be up to a second inaccurate.
--
--   Moreover, it takes in an Ethernet stream with the ARP
--   etherType (0x0806), and updates the ARP table upon receiving a valid ARP
--   reply or gratitious ARP request. Gratitious ARP replies are ignored for now.
--   If a normal ARP request is received, it transmits a reply.
--
--   Does not support Proxy ARP.
arpC
  :: forall
     (dom :: Domain)
     (maxAgeSeconds :: Nat)
     (maxWaitSeconds :: Nat)
     (dataWidth :: Nat)
   . HiddenClockResetEnable dom
  => KnownNat dataWidth
  => KnownNat (DomainPeriod dom)
  => DomainPeriod dom <= 5 * 10^11
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
  -> Circuit (PacketStream dom dataWidth EthernetHeader, ArpLookup dom)
             (PacketStream dom dataWidth EthernetHeader)
arpC maxAge maxWait ourMacS ourIPv4S =
  -- TODO waiting for an ARP reply in seconds is too coarse.
  -- Make this timer less coarse, e.g. milliseconds
  circuit $ \(ethStream, lookupIn) -> do
    (entry, replyOut) <- arpReceiverC ourIPv4S -< ethStream
    (lookupOut, requestOut) <- arpManagerC maxWait -< lookupIn
    () <- arpTable maxAge -< (lookupOut, entry)
    arpPktOut <- Df.roundrobinCollect Df.Skip -< [replyOut, requestOut]
    arpTransmitterC ourMacS ourIPv4S -< arpPktOut
