{-|
Module      : Clash.Cores.Arp.ArpTable
Description : Provides a highly configurable ARP table.
-}

{-# language FlexibleContexts #-}
{-# language RecordWildCards #-}

module Clash.Cores.Ethernet.Arp.ArpTable
  ( arpTable
  ) where

import Clash.Prelude
import Clash.Signal.Extra

import Protocols
import Protocols.Df qualified as Df

import Clash.Cores.Ethernet.Arp.ArpTypes
import Clash.Cores.Ethernet.IP.IPv4Types

import Data.Maybe


data ArpTableState depth
  = Active {
    -- | Whether the output of the blockram contains valid data
    _bramValid :: Bool
  }
  -- ^ The ARP table is handling insertion and lookup requests
  | Invalidating {
    -- | The timer of the entry at this address will be decremented
    _writeAddr :: Unsigned depth
  }
  -- ^ The ARP table is decrementing the timers of all entries,
  --   and therefore cannot currently accept any insertion or lookup requests.
  deriving (Generic, Show, ShowX, NFDataX)

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
  -> ( Bool
     , (ArpEntry, Index (maxAgeSeconds + 1))
     , Maybe (ArpEntry, Unsigned depth)
     , Maybe (IPv4Address, Unsigned depth)
     , Bool
     )
  -> ( ArpTableState depth
     , ( Ack
       , Unsigned depth
       , Maybe (Unsigned depth, (ArpEntry, Index (maxAgeSeconds + 1)))
       , Maybe ArpResponse
       )
     )
-- If the reset is enabled, go back to the initial state
-- and don't acknowledge or send out data.
arpTableT _ (True, _, _, _, _) =
  (Active False, (Ack False, 0, Nothing, Nothing))

arpTableT Active{..} (_, (arpEntry, secsLeft), insertionWithHash, lookupWithHash, secondPassed)
  = (nextSt, (Ack True, readAddr, writeCmd, arpResponseOut))
    where
      writeCmd = (\(entry, hash) -> (hash, (entry, maxBound))) <$> insertionWithHash
      validLookup = isJust lookupWithHash

      arpResponseOut
        | _bramValid && validLookup = Just (arpResponse (fst $ fromJustX lookupWithHash))
        | otherwise = Nothing

      -- It is possible that the IP stored in the entry is not the same as the lookup IP.
      -- This happens due to hash collisions.
      arpResponse lookupIP =
        if secsLeft == 0 || lookupIP /= _arpIP arpEntry
        then ArpEntryNotFound
        else ArpEntryFound (_arpMac arpEntry)

      (nextSt, readAddr)
        | secondPassed = (Invalidating maxBound, maxBound)
        | otherwise = (Active (validLookup && not _bramValid), maybe 0 snd lookupWithHash)

arpTableT Invalidating{..} (_, (arpEntry, secsLeft), _, _, _)
  = (nextSt, (Ack False, readAddr, writeCmd, Nothing))
    where
      writeCmd = Just (_writeAddr, (arpEntry, satPred SatBound secsLeft))
      (nextSt, readAddr)
        | _writeAddr == 0 = (Active False, 0)
        | otherwise = let addr = pred _writeAddr in (Invalidating addr, addr)

-- | ARP table that stores @2^depth@ entries in block ram. `maxAgeSeconds` is the number of seconds before the
--   entry will be removed from the table (lazily). The timeout is inaccurate for up to one second, because
--   the circuit uses a constant counter for efficiency. Every second, the ARP table is unable to handle insertion
--   and lookup requests for @2^depth@ clock cycles, because it needs to decrease the timers of the entries.
--   During this period, the component will assert backpressure. Note that this implies that the component will
--   not work correctly when the size of the ARP table is bigger than the clock frequency.
--
--   An entry may be evicted sooner than expected from the cache due to hash collisions; entries are addressed
--   by taking the last `depth` bits of their corresponding IPv4 address. By increasing the
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
  => 1 <= maxAgeSeconds
  => 1 <= depth
  => depth <= 32
  => SNat depth
  -- ^ Determines the number of entries in the ARP table, namely @2^depth@.
  -> SNat maxAgeSeconds
  -- ^ Entries are no longer valid after this number of seconds, starting at the time of insertion.
  -> Circuit (ArpLookup dom, Df dom ArpEntry) ()
  -- ^ First of LHS is a MAC lookup request for that IPv4 address.
  --   Second of LHS is an insertion request.
arpTable SNat SNat = Circuit (hideReset ckt)
  where
    ckt reset ((lookupReq, insertReq), ()) = ((arpResponse, outReady), ())
      where
        -- The underlying blockram.
        tableEntry = blockRam1 NoClearOnReset (SNat @(2^depth)) (errorX "", 0) readAddr writeCmd

        -- Hashes of the IPv4 addresses, used to address the blockram.
        -- We simply take the last `depth` bits of the IPv4 address.
        lookupWithHash :: Signal dom (Maybe (IPv4Address, Unsigned depth))
        lookupWithHash = fmap (\ipAddr -> (ipAddr, resize $ bitCoerce ipAddr)) <$> lookupReq

        insertionWithHash :: Signal dom (Maybe (ArpEntry, Unsigned depth))
        insertionWithHash = fmap (\entry -> (entry, resize $ bitCoerce (_arpIP entry))) <$> fmap Df.dataToMaybe insertReq

        readAddr :: Signal dom (Unsigned depth)
        writeCmd :: Signal dom (Maybe (Unsigned depth, (ArpEntry, Index (maxAgeSeconds + 1))))
        (outReady, readAddr, writeCmd, arpResponse) =
          unbundle (mealy arpTableT (Active False) input)

        input = bundle
          ( unsafeToActiveHigh reset
          , tableEntry
          , insertionWithHash
          , lookupWithHash
          , secondTimer
          )
