{-# language FlexibleContexts #-}

{-|
Module      : Clash.Cores.Arp.ArpTable
Description : Provides an ARP table which is able to hold one ARP entry.
-}
module Clash.Cores.Arp.ArpTable
  (arpTable) where

import Clash.Prelude

import Protocols
import Protocols.Df hiding ( pure )

import Clash.Cores.Arp.ArpTypes


-- | This register is 0 exactly every second.
--   We might want to export this in the future if other components need this as well.
secondTimer
  :: forall (dom :: Domain) .
  ( HiddenClockResetEnable dom
  , 1 <= DomainPeriod dom
  , 1 <= 10^12 `Div` DomainPeriod dom
  , KnownNat (DomainPeriod dom))
  => Signal dom (Index (10^12 `Div` DomainPeriod dom))
secondTimer = register maxBound (satPred SatWrap <$> secondTimer)

-- | ARP table that stores one ARP entry in a register. `maxAgeSeconds` is the number of seconds before the
--   entry will be removed from the table (lazily). The timeout is inaccurate for up to one second, because
--   the circuit uses a constant counter for efficiency.
arpTable
  :: forall (dom :: Domain) (maxAgeSeconds :: Nat) .
  ( HiddenClockResetEnable dom
  , 1 <= maxAgeSeconds
  , 1 <= DomainPeriod dom
  , 1 <= 10^12 `Div` DomainPeriod dom
  , KnownNat (DomainPeriod dom))
  => SNat maxAgeSeconds
  -> Circuit (ArpLookup dom, Df dom ArpEntry) ()
  -- ^ First LHS is a MAC request for that IP. Second of LHS is an insertion request
arpTable SNat = fromSignals ckt
  where
    ckt ((macReq, insertReq), ()) = ((arpResponse, pure (Ack True)), ())
      where
        arpEntry :: Signal dom (ArpEntry, Index (maxAgeSeconds + 1))
        arpEntry = register (errorX "empty initial content", 0) writeCommand

        writeCommand = fmap go (bundle (insertReq, bundle (arpEntry, secondTimer)))
          where
            go (req, ((entry, secs), cnt)) = case req of
              NoData -> (entry, if cnt == 0 then satPred SatBound secs else secs)
              Data reqEntry -> (reqEntry, maxBound)

        arpResponse = fmap go (bundle (macReq, arpEntry))
          where
            go (ip, (entry, timeLeft)) = ip >>= \ipAddr ->
              if timeLeft == 0 || _arpIP entry /= ipAddr
              then Just ArpEntryNotFound
              else Just (ArpEntryFound (_arpMac entry))
