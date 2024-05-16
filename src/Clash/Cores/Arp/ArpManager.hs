{-|
Module      : Clash.Cores.Arp.ArpManager
Description : TODO
-}

{-# language FlexibleContexts #-}
{-# language RecordWildCards #-}

module Clash.Cores.Arp.ArpManager where

import Clash.Prelude

import Protocols
import Protocols.Df

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

-- | TODO
data ArpManagerState maxWaitSeconds
  = AwaitLookup
  | AwaitArpReply {
    _secondsLeft :: Index maxWaitSeconds
  } deriving (Generic, NFDataX, Show, ShowX)

-- | TODO
arpManagerT
  :: forall (maxWaitSeconds :: Nat)
   . 1 <= maxWaitSeconds
  => KnownNat maxWaitSeconds
  => ArpManagerState maxWaitSeconds
  -> (Maybe IPv4Address, Data ArpEntry, Maybe ArpResponse, Ack, Bool)
  -> (ArpManagerState maxWaitSeconds
     , ((Maybe ArpResponse, Ack), (Maybe IPv4Address, Data ArpLite)))
-- User issues a lookup request. Always acknowledge, because in this state we don't care incoming ARP replies.
-- We don't have a timeout, because the ARP table should always respond within a reasonable time frame.
-- If not, there is a bug in the ARP table.
arpManagerT AwaitLookup (Just lookupIPv4, _, arpResponse, Ack readyIn, _)
  = (nextStOut, ((arpResponseOut, Ack True), (lookupOut, arpRequestOut)))
    where
      (arpResponseOut, arpRequestOut, lookupOut, nextSt) = case arpResponse of
        Nothing                -> (Nothing, NoData, Just lookupIPv4, AwaitLookup)
        Just ArpEntryNotFound  -> (Nothing, Data (ArpLite broadcastMac lookupIPv4 True), Nothing, AwaitArpReply maxBound)
        Just (ArpEntryFound _) -> (arpResponse, NoData, Nothing, AwaitLookup)

      nextStOut = if readyIn then nextSt else AwaitLookup

-- We don't care about backpressure, because we do not send ARP requests.
arpManagerT AwaitArpReply{..} (Just lookupIPv4, entry, _, _, secondPassed)
  = (nextSt, ((arpResponseOut, Ack True), (Nothing, NoData)))
    where
      newTimer = if secondPassed then pred _secondsLeft else _secondsLeft

      (nextSt, arpResponseOut)
        = case (entry, _secondsLeft == 0) of
          (NoData, True)   -> (AwaitLookup, Just ArpEntryNotFound)
          (NoData, False)  -> (AwaitArpReply newTimer, Nothing)
          (Data e, timeUp) -> if _arpIP e == lookupIPv4
                             then (AwaitLookup, Just (ArpEntryFound $ _arpMac e))
                             else (if timeUp then AwaitLookup else AwaitArpReply newTimer, Nothing)

arpManagerT st (Nothing, _, _, ackIn, _) = (st, ((Nothing, ackIn), (Nothing, NoData)))

-- | TODO
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
  -> Circuit (ArpLookup dom, Df dom ArpEntry) (ArpLookup dom, Df dom ArpLite)
arpManagerC SNat = fromSignals ckt
  where
    ckt :: ((Signal dom (Maybe IPv4Address), Signal dom (Data ArpEntry)),
            (Signal dom (Maybe ArpResponse), Signal dom Ack))
           ->
           ((Signal dom (Maybe ArpResponse), Signal dom Ack),
            (Signal dom (Maybe IPv4Address), Signal dom (Data ArpLite)))
    ckt ((s1, s2), (s3, s4)) = (unbundle s5, unbundle s6)
      where
        (s5, s6) = mealyB arpManagerT (AwaitLookup @maxWaitSeconds) (s1, s2, s3, s4, secondTimer)
