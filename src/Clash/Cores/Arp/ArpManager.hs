{-|
Module      : Clash.Cores.Arp.ArpManager
Description : Provides an ARP manager which handles ARP lookups from client circuits.
-}

{-# language FlexibleContexts #-}
{-# language RecordWildCards #-}

module Clash.Cores.Arp.ArpManager where

import Clash.Prelude

import Protocols
import Protocols.Df hiding ( pure )

import Clash.Cores.Arp.ArpTypes
import Clash.Cores.Ethernet.EthernetTypes
import Clash.Cores.IP.IPv4Types


-- | This register is @True@ exactly every second.
secondTimer
  :: forall (dom :: Domain)
   . HiddenClockResetEnable dom
  => 1 <= DomainPeriod dom
  => 1 <= 10^12 `Div` DomainPeriod dom
  => KnownNat (DomainPeriod dom)
  => Signal dom Bool
secondTimer = isRising 0 $ msb <$> counter
  where
    counter :: Signal dom (Index (10^12 `Div` DomainPeriod dom))
    counter = register maxBound (satPred SatWrap <$> counter)

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
  -> (Maybe IPv4Address, Data ArpEntry, Maybe ArpResponse, Ack, Bool)
  -> (ArpManagerState maxWaitSeconds
     , (Maybe ArpResponse, (Maybe IPv4Address, Data ArpLite)))
-- User issues a lookup request. Always acknowledge, because in this state we don't care incoming ARP replies.
-- We don't have a timeout, because the ARP table should always respond within a reasonable time frame.
-- If not, there is a bug in the ARP table.
arpManagerT AwaitLookup (Just lookupIPv4, _, arpResponseIn, Ack readyIn, _)
  = (nextSt, (arpResponseOut, (lookupOut, arpRequestOut)))
    where
      (arpResponseOut, arpRequestOut, lookupOut, nextSt) = case arpResponseIn of
        Nothing
          -> ( Nothing
             , NoData
             , Just lookupIPv4
             , AwaitLookup
             )
        Just ArpEntryNotFound
          -> ( Nothing
             , Data (ArpLite broadcastMac lookupIPv4 True)
             , Nothing
             , if readyIn then AwaitArpReply maxBound else AwaitLookup
             )
        Just (ArpEntryFound _)
          -> ( arpResponseIn
             , NoData
             , Nothing
             , AwaitLookup
             )

-- We don't care about incoming backpressure, because we do not send ARP requests in this state.
-- We always acknowledge incoming ARP replies.
arpManagerT AwaitArpReply{..} (Just lookupIPv4, entry, _, _, secondPassed)
  = (nextSt, (arpResponseOut, (Nothing, NoData)))
    where
      newTimer = if secondPassed then pred _secondsLeft else _secondsLeft

      (nextSt, arpResponseOut)
        = case (entry, _secondsLeft == 0) of
          (_, True)
            -> (AwaitLookup, Just ArpEntryNotFound)
          (NoData, _)
            -> (AwaitArpReply newTimer, Nothing)
          (Data e, _)
            -> if _arpIP e == lookupIPv4
               then (AwaitLookup, Just (ArpEntryFound $ _arpMac e))
               else (AwaitArpReply newTimer, Nothing)

arpManagerT st (Nothing, _, _, _, _) = (st, (Nothing, (Nothing, NoData)))

-- | This component handles ARP lookup requests by client components. If a lookup IPv4 address is not found
--   in the ARP table, it will broadcast an ARP request to the local network and wait at most `maxWaitSeconds`
--   for a reply. If no reply was received within time, the lookup request is ignored. `maxWaitSeconds` is inaccurate
--   for up to one second less. For example, if `maxWaitSeconds` ~ 30, then the component will wait for 29-30 seconds.
arpManagerC
  :: forall (dom :: Domain)
            (maxWaitSeconds :: Nat)
   . HiddenClockResetEnable dom
  => KnownDomain dom
  => 1 <= DomainPeriod dom
  => 1 <= 10^12 `Div` DomainPeriod dom
  => KnownNat (DomainPeriod dom)
  => 1 <= maxWaitSeconds
  => SNat maxWaitSeconds
  -- ^ The amount of seconds we wait for an incoming ARP reply
  -> Circuit (ArpLookup dom, Df dom ArpEntry) (ArpLookup dom, Df dom ArpLite)
arpManagerC SNat = fromSignals ckt
  where
    ckt ((lookupIPv4S, entryS), (arpResponseInS, ackInS)) = ((bwdOut, pure $ Ack True), unbundle fwdOut)
      where
        (bwdOut, fwdOut)
          = mealyB arpManagerT (AwaitLookup @maxWaitSeconds) (lookupIPv4S, entryS, arpResponseInS, ackInS, secondTimer)
