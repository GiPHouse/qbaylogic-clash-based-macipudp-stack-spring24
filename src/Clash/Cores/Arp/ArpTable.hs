{-|
Module      : Clash.Cores.Arp.ArpTable
Description : Provides an ARP table which is able to hold one ARP entry.
-}

{-# language FlexibleContexts #-}

module Clash.Cores.Arp.ArpTable
  ( arpTable
  , secondTimer
  ) where

import Clash.Prelude

import Protocols
import Protocols.Df qualified as Df

import Clash.Cores.Arp.ArpTypes


-- | This register is @True@ exactly every second.
secondTimer
  :: forall (dom :: Domain)
   . HiddenClockResetEnable dom
  => KnownNat (DomainPeriod dom)
  => 1 <= DomainPeriod dom
  => Signal dom Bool
secondTimer = case compareSNat d1 (SNat @(10^12 `Div` DomainPeriod dom)) of
  SNatLE -> isRising 0 $ msb <$> counter
    where
      counter :: Signal dom (Index (10^12 `Div` DomainPeriod dom))
      counter = register maxBound (satPred SatWrap <$> counter)
  SNatGT -> errorX "secondTimer: Absurd, Report this to the Clash compiler team: https://github.com/clash-lang/clash-compiler/issues"

-- | ARP table that stores one ARP entry in a register. `maxAgeSeconds` is the number of seconds before the
--   entry will be removed from the table (lazily). The timeout is inaccurate for up to one second less, because
--   the circuit uses a constant counter for efficiency. For example, when `maxAgeSeconds` is set to 30,
--   an entry will expire in 29-30 seconds.
arpTable
  :: forall
    (dom :: Domain)
    (maxAgeSeconds :: Nat)
   . HiddenClockResetEnable dom
  => KnownNat (DomainPeriod dom)
  => 1 <= DomainPeriod dom
  => 1 <= maxAgeSeconds
  => SNat maxAgeSeconds
  -- ^ The ARP entry will expire after this many seconds
  -> Circuit (ArpLookup dom, Df dom ArpEntry) ()
  -- ^ First of LHS is a MAC address request for the given IPv4 address. Second of LHS is an insertion request
arpTable SNat = fromSignals ckt
  where
    ckt ((lookupReq, insertReq), ()) = ((arpResponse, pure (Ack True)), ())
      where
        arpEntry :: Signal dom (ArpEntry, Index (maxAgeSeconds + 1))
        arpEntry = register (errorX "empty initial content", 0) writeCommand

        writeCommand = fmap go (bundle (insertReq, arpEntry, secondTimer))
          where
            go (req, (entry, secondsLeft), secondPassed) = case req of
              Df.NoData -> (entry, if secondPassed then satPred SatBound secondsLeft else secondsLeft)
              Df.Data reqEntry -> (reqEntry, maxBound)

        arpResponse = fmap go (bundle (lookupReq, arpEntry))
          where
            go (ip, (entry, timeLeft)) = ip >>= \ipAddr ->
              if timeLeft == 0 || _arpIP entry /= ipAddr
              then Just ArpEntryNotFound
              else Just (ArpEntryFound (_arpMac entry))
