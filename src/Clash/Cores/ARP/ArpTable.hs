{-# language FlexibleContexts #-}

{-|
Module      : Clash.Cores.ARP.ArpTable
Description : Provides an ARP table which is able to hold one ARP entry.
-}
module Clash.Cores.ARP.ArpTable
  (arpTable) where

import Clash.Prelude

import Protocols
import Protocols.Df hiding ( pure )

import Clash.Cores.ARP.ArpTypes

import Data.Maybe


-- | ARP table that stores one ARP entry in a register. `maxAge` is the number of clock cycles before this
--   entry will be removed from the table. To translate this to seconds for a specific domain `dom`,
--   use the formula @seconds * 10^12 `Div` (DomainPeriod dom)@.
arpTable
  :: forall (dom :: Domain) (maxAge :: Nat) .
  ( HiddenClockResetEnable dom
  , 1 <= maxAge)
  => SNat maxAge
  -> Circuit (ARPLookup dom, Df dom ARPEntry) ()
  -- ^ First LHS is a MAC request for that IP. Second of LHS is an insertion request
arpTable SNat = fromSignals ckt
  where
    ckt ((macReq, insertReq), ()) = ((arpResponse, pure (Ack True)), ())
      where
        -- A counter to record timestamps of ARP entries.
        -- 64 bits is enough for >10000 years before overflow on a 50 MHz FPGA clock.
        counter :: Signal dom (Unsigned 64)
        counter = register 0 (counter + 1)

        arpEntry :: Signal dom (Data (ARPEntry, Unsigned 64))
        arpEntry = register NoData (mux (isJust <$> writeCommand) (fromJust <$> writeCommand) arpEntry)

        -- If this is a Just, overwrite the current ARP entry. Else, keep the current ARP entry.
        writeCommand = fmap go (bundle (insertReq, bundle (arpEntry, counter)))
          where
            go (req, (entry, cnt)) = case req of
              NoData -> case entry of
                NoData -> Nothing
                Data (_, timestamp) -> if cnt - timestamp >= (natToNum @maxAge) then Just NoData else Nothing
              Data reqEntry -> Just (Data (reqEntry, cnt))

        arpResponse = fmap go (bundle (macReq, arpEntry))
          where
            go (ip, entry) = case ip of
              -- No request, so no response
              Nothing     -> Nothing 
              -- Request. Now we have to look if the IP address is in our ARP table
              Just ipAddr -> case entry of 
                NoData -> Just ARPEntryNotFound
                Data (e, _) -> if _arpIP e == ipAddr then Just $ ARPEntryFound (_arpMac e) else Just ARPEntryNotFound
