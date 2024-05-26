{-# language FlexibleContexts #-}
{-# language RecordWildCards #-}

{-|
Module      : Clash.Cores.Ethernet.Arp.ArpManager
Description : Provides an ARP manager which handles ARP lookups from client circuits.
-}
module Clash.Cores.Ethernet.Arp.ArpManager where

import Clash.Prelude
import Clash.Signal.Extra

import Protocols
import Protocols.Df qualified as Df

import Clash.Cores.Ethernet.Arp.ArpTypes
import Clash.Cores.Ethernet.IP.IPv4Types
import Clash.Cores.Ethernet.Mac.EthernetTypes


-- | State of the ARP manager.
data ArpManagerState maxWaitSeconds
  = AwaitLookup
  | AwaitArpReply {
    _secondsLeft :: Index (maxWaitSeconds + 1)
  } deriving (Generic, NFDataX, Show, ShowX)

-- | ARP manager transition function.
arpManagerT
  :: forall (maxWaitSeconds :: Nat)
   . 1 <= maxWaitSeconds
  => KnownNat maxWaitSeconds
  => ArpManagerState maxWaitSeconds
  -> (Maybe IPv4Address, Maybe ArpResponse, Ack, Bool)
  -> (ArpManagerState maxWaitSeconds
     , (Maybe ArpResponse, (Maybe IPv4Address, Df.Data ArpLite)))
-- User issues a lookup request. We don't have a timeout, because the ARP table should
-- always respond within a reasonable time frame. If not, there is a bug in the ARP table.
arpManagerT AwaitLookup (Just lookupIPv4, arpResponseIn, Ack readyIn, _) =
  (nextSt, (arpResponseOut, (Just lookupIPv4, arpRequestOut)))
    where
      (arpResponseOut, arpRequestOut, nextSt) = case arpResponseIn of
        Nothing
          -> ( Nothing
             , Df.NoData
             , AwaitLookup
             )
        Just ArpEntryNotFound
          -> ( Nothing
             , Df.Data (ArpLite broadcastMac lookupIPv4 True)
             , if readyIn then AwaitArpReply maxBound else AwaitLookup
             )
        Just (ArpEntryFound _)
          -> ( arpResponseIn
             , Df.NoData
             , AwaitLookup
             )

-- We don't care about incoming backpressure, because we do not send ARP requests in this state.
-- We keep polling the ARP table until either a timeout occurs or the entry is found.
-- This requires the ARP table to handle read and write requests in parallel.
arpManagerT AwaitArpReply{..} (Just lookupIPv4, arpResponseIn, _, secondPassed) =
  (nextSt, (arpResponseOut, (Just lookupIPv4, Df.NoData)))
    where
      newTimer = if secondPassed then satPred SatBound _secondsLeft else _secondsLeft

      (arpResponseOut, nextSt) =
        case (arpResponseIn, _secondsLeft == 0) of
          (Just (ArpEntryFound _), _)
            -> (arpResponseIn, AwaitLookup)
          (Just ArpEntryNotFound, True)
            -> (arpResponseIn, AwaitLookup)
          -- Note that we keep driving the same lookup request when the ARP table has not acknowledged
          -- our request yet, even if the time is up. If we don't, we violate protocol invariants.
          -- Therefore timer can be slightly inaccurate, depending on the latency of the ARP table.
          (_, _)
            -> (Nothing, AwaitArpReply newTimer)

arpManagerT st (Nothing, _,  _, _) = (st, (Nothing, (Nothing, Df.NoData)))

-- | This component handles ARP lookup requests by client components. If a lookup IPv4 address is not found
--   in the ARP table, it will broadcast an ARP request to the local network and wait at most `maxWaitSeconds`
--   for a reply. If no reply was received within time, the lookup request is ignored. `maxWaitSeconds` is inaccurate
--   for up to one second less. For example, if `maxWaitSeconds` ~ 30, then the component will wait for 29-30 seconds.
--   Does not support clock frequencies lower than 2 Hz.
arpManagerC
  :: forall (dom :: Domain)
            (maxWaitSeconds :: Nat)
   . HiddenClockResetEnable dom
  => KnownDomain dom
  => KnownNat (DomainPeriod dom)
  => 1 <= DomainPeriod dom
  => DomainPeriod dom <= 5 * 10^11
  => 1 <= maxWaitSeconds
  => SNat maxWaitSeconds
  -- ^ The amount of seconds we wait for an incoming ARP reply
  -> Circuit (ArpLookup dom) (ArpLookup dom, Df dom ArpLite)
arpManagerC SNat = fromSignals ckt
  where
    ckt (lookupIPv4S, (arpResponseInS, ackInS)) = (bwdOut, unbundle fwdOut)
      where
        (bwdOut, fwdOut) =
          mealyB arpManagerT (AwaitLookup @maxWaitSeconds) (lookupIPv4S, arpResponseInS, ackInS, secondTimer)
