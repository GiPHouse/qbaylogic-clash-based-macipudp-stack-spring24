{-|
Module      : Clash.Cores.Arp.ArpTable
Description : Provides a highly configurable ARP table.
-}

{-# language FlexibleContexts #-}
{-# language MultiParamTypeClasses #-}
{-# language RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Clash.Cores.Ethernet.Arp.ArpTable
  ( arpTable
  ) where

import Clash.Prelude
import Clash.Signal.Extra

import Protocols
import Protocols.Df qualified as Df

import Clash.Cores.Crc
import Clash.Cores.Crc.Catalog
import Clash.Cores.Ethernet.Arp.ArpTypes
import Clash.Cores.Ethernet.IP.IPv4Types

import Data.Bifunctor
import Data.Proxy


$(deriveHardwareCrc (Proxy @Crc32_ethernet) d8 d4)

data ArpTableState depth
  = Initializing
  -- ^ Registers and block ram need to be written to before they can be read,
  --   so we do nothing for one clock cycle
  | Active {
    _stallArpResponse :: Bool
    -- ^ After invalidating, the block ram output should not be used to send out an ARP response
  }
  -- ^ The ARP table is waiting for insertion and lookup requests
  | Invalidating {
  -- ^ The ARP table is decrementing the timers of all entries.
    _tableIdx :: Index (2^depth)
  } deriving (Generic, Show, ShowX, NFDataX)

arpTableT
  :: forall
     (depth :: Nat)
     (maxAgeSeconds :: Nat)
   . KnownNat depth
  => KnownNat maxAgeSeconds
  => 1 <= maxAgeSeconds
  => 1 <= depth
  => depth <= 32
  => ArpTableState depth
  -> ( Maybe (ArpEntry, Index (maxAgeSeconds + 1))
     , Maybe ArpEntry
     , BitVector depth
     , Maybe IPv4Address
     , BitVector depth
     , Bool
     )
  -> ( ArpTableState depth
     , ( Ack
       , Index (2^depth)
       , Maybe (Index (2^depth)
       , Maybe (ArpEntry, Index (maxAgeSeconds + 1)))
       , Maybe ArpResponse
       )
     )
arpTableT Initializing _ = (Active True, (Ack False, 0, Nothing, Nothing))

arpTableT Active{..} (bramOtp, insertReq, insertCrc, lookupReq, lookupCrc, secondPassed)
  = (nextSt, (Ack True, readAddr, writeCmd, arpResponse))
    where
      (nextSt, readAddr)
        = if secondPassed
          then (Invalidating maxBound, maxBound)
          else (Active False, bitCoerce lookupCrc)
      writeCmd = (\entry -> (bitCoerce insertCrc, Just (entry, maxBound))) <$> insertReq
      arpResponse = if _stallArpResponse then Nothing else newArpResponse <$> lookupReq

      newArpResponse lookupIP = case bramOtp of
        Nothing -> ArpEntryNotFound
        Just (entry, secsLeft) ->
          -- It is possible that the IP stored in the entry is not the same as the lookup IP.
          -- This happens due to CRC collisions.
          if secsLeft == 0 || lookupIP /= _arpIP entry
          then ArpEntryNotFound
          else ArpEntryFound (_arpMac entry)

arpTableT Invalidating{..} (bramOtp, _, _, _, lookupCrc, _)
  = (nextSt, (Ack False, readAddr, writeCmd, Nothing))
    where
      (nextSt, readAddr)
        = if _tableIdx == 0
          then (Active True, bitCoerce lookupCrc)
          else (Invalidating (pred _tableIdx), pred _tableIdx)
      writeCmd = fmap (\entry -> (_tableIdx, Just entry)) adjustedArpEntry
      adjustedArpEntry = second (satPred SatBound) <$> bramOtp

-- | ARP table that stores @2^depth@ entries in block ram. `maxAgeSeconds` is the number of seconds before the
--   entry will be removed from the table (lazily). The timeout is inaccurate for up to one second, because
--   the circuit uses a constant counter for efficiency. Every second, the ARP table is unable to handle insertion
--   and lookup requests for @2^depth@ clock cycles, because it needs to decrease the timers of the entries.
--   During this period, the component will assert backpressure. Note that this implies that the component will
--   not work correctly when the size of the ARP table is bigger than the clock frequency.
--
--   An entry may be evicted sooner than expected from the cache due to CRC collisions; entries are addressed
--   by hashing the IPv4 address with CRC-32 and taking the last `depth` bits of this hash. By increasing the
--   number of entries in the table, the chance of IPv4 addresses colliding is lower.
arpTable
  :: forall
     (dom :: Domain)
     (depth :: Nat)
     (maxAgeSeconds :: Nat)
   . HiddenClockResetEnable dom
  => KnownDomain dom
  => 1 <= DomainPeriod dom
  => DomainPeriod dom <= 5 * 10^11
  => KnownNat (DomainPeriod dom)
  => HardwareCrc Crc32_ethernet 8 4
  => 1 <= maxAgeSeconds
  => 1 <= depth
  => depth <= 32
  => SNat depth
  -- ^ Determines the number of entries in the ARP table, namely @2^depth@
  -> SNat maxAgeSeconds
  -- ^ After this amount of seconds, entries are no longer valid
  -> Circuit (ArpLookup dom, Df dom ArpEntry) ()
  -- ^ First LHS is a MAC request for that IP. Second of LHS is an insertion request
arpTable SNat SNat = fromSignals ckt
  where
    ckt ((lookupReq, insertReq), ()) = ((arpResponse, outReady), ())
      where
        -- We need to store insertion and lookup requests from last clock cycle,
        -- because the CRC output is delayed by one cycle.
        insertReqLast = register Nothing (Df.dataToMaybe <$> insertReq)
        lookupReqLast = register Nothing lookupReq

        -- Lookup requests are delayed by 2 clock cycles. This delay is necessary in order to
        -- align the lookup request and the blockram output, because the CRC causes a delay of 1 cycle
        -- and the blockram causes a delay of 1 cycle as well.
        lookupReqLast2 = register Nothing lookupReqLast

        crcLookupS, crcInsertS  :: Signal dom (Maybe (Index 4, Vec 4 (BitVector 8)))
        crcLookupS = fmap (fmap (\ip -> (maxBound, bitCoerce ip))) lookupReq
        crcInsertS = fmap (fmap (\entry -> (maxBound, bitCoerce $ _arpIP entry))) (Df.dataToMaybe <$> insertReq)

        -- CRC-32 hashes of the IPv4 addresses for addressing purposes.
        -- Resets are constantly asserted, because we only need one clock cycle to calculate the CRC.
        crcLookupIP = resize <$> crcEngine (Proxy @Crc32_ethernet) (pure True) crcLookupS
        crcInsertIP = resize <$> crcEngine (Proxy @Crc32_ethernet) (pure True) crcInsertS

        readAddr :: Signal dom (Index (2^depth))
        writeCmd :: Signal dom (Maybe (Index (2^depth), Maybe (ArpEntry, Index (maxAgeSeconds + 1))))
        (outReady, readAddr, writeCmd, arpResponse) = unbundle (mealy arpTableT Initializing mealyInp)
        mealyInp = bundle (cacheEntry, insertReqLast, crcInsertIP, lookupReqLast2, crcLookupIP, secondTimer)
        cacheEntry = blockRam1 NoClearOnReset (SNat @(2^depth)) Nothing readAddr writeCmd
