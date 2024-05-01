{-|
Module      : Clash.Cores.ARP.ArpTypes
Description : Provides various data types, aliases and constants for the Address Resolution Protocol.
-}
module Clash.Cores.ARP.ArpTypes where

import Clash.Cores.Ethernet.EthernetTypes
import Clash.Prelude

import Protocols


newtype IPAddress = IPAddress (Vec 4 (BitVector 8))
  deriving (Generic, Show, ShowX, NFDataX, Eq, BitPack)

-- | An entry for our ARP table, which maps an IP address to a MAC address.
--   A timestamp should be kept separately from this type.
data ARPEntry
  = ARPEntry {
    _arpMac :: MacAddress,
    _arpIP :: IPAddress
    } deriving (Generic, Show, ShowX, NFDataX)

-- | An ARP response. Either the IP address is not found in the table, or it is and its
--   corresponding MAC address is returned.
data ARPResponse = ARPEntryNotFound | ARPEntryFound MacAddress
  deriving (Generic, Show, ShowX, NFDataX, Eq)

data ARPLookup (dom :: Domain)

instance Protocol (ARPLookup dom) where
  type Fwd (ARPLookup dom) = Signal dom (Maybe IPAddress)
  type Bwd (ARPLookup dom) = Signal dom (Maybe ARPResponse)
