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

import Data.Maybe


-- | ARP table that stores one ARP entry in a register. `maxAge` is the number of clock cycles before this
--   entry will be removed from the table. To translate this to seconds for a specific domain `dom`,
--   use the formula @seconds * 10^12 `Div` (DomainPeriod dom)@.
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
        -- A second has passed if this counter is 0. The counter is reset upon an insertion request.
        counter :: Signal dom (Index (10^12 `Div` DomainPeriod dom))
        counter = withReset rst register maxBound (satPred SatWrap <$> counter)
        rst = unsafeFromLowPolarity (isJust . dataToMaybe <$> insertReq)

        arpEntry :: Signal dom (ArpEntry, Index (maxAgeSeconds + 1))
        arpEntry = register (errorX "empty initial content", 0) writeCommand

        writeCommand = fmap go (bundle (insertReq, bundle (arpEntry, counter)))
          where
            go (req, ((entry, secs), cnt)) = case req of
              NoData -> (entry, if cnt == 0 then satPred SatBound secs else secs)
              Data reqEntry -> (reqEntry, maxBound)

        arpResponse = fmap go (bundle (macReq, arpEntry))
          where
            go (ip, (entry, timeLeft)) = case ip of
              -- No request, so no response
              Nothing     -> Nothing
              -- Request. Now we have to look if the IP address is in our ARP table
              Just ipAddr -> if timeLeft == 0 || _arpIP entry /= ipAddr
                             then Just ArpEntryNotFound
                             else Just (ArpEntryFound (_arpMac entry))
