{-|
Module      : Clash.Cores.Arp.ArpTypes
Description : Provides various data types, aliases, constructors and constants for the Address Resolution Protocol.
              This module only provides the most common use case of ARP, which is mapping IPv4 addresses to MAC addresses.
-}

module Clash.Cores.Arp.ArpTypes where

import Clash.Cores.Ethernet.EthernetTypes
import Clash.Cores.IP.IPv4Types
import Clash.Prelude

import Protocols


-- | An entry for our ARP table, which maps an IPv4 address to a MAC address.
--   A timestamp should be kept separately from this type.
data ArpEntry
  = ArpEntry {
    _arpMac :: MacAddress,
    _arpIP :: IPAddress
    } deriving (Generic, Show, ShowX, NFDataX)

-- | An ARP response. Either the IPv4 address is not found in the table, or it is and its
--   corresponding MAC address is returned.
data ArpResponse = ArpEntryNotFound | ArpEntryFound MacAddress
  deriving (Generic, Show, ShowX, NFDataX, Eq)

data ArpLookup (dom :: Domain)

instance Protocol (ArpLookup dom) where
  type Fwd (ArpLookup dom) = Signal dom (Maybe IPAddress)
  type Bwd (ArpLookup dom) = Signal dom (Maybe ArpResponse)

-- | ARP packet structure. The first four fields are constant for our use case.
data ArpPacket
  = ArpPacket {
    _htype :: BitVector 16,
    -- ^ Hardware type. 0x0001 for Ethernet.
    _ptype :: BitVector 16,
    -- ^ Protocol type. 0x0800 for IPv4.
    _hlen :: BitVector 8,
    -- ^ Length of the hardware adresses. 0x06 for Ethernet.
    _plen :: BitVector 8,
    -- ^ Length of the protocol (internet) addresses. 0x04 for IPv4.
    _oper :: BitVector 16,
    -- ^ Operation that the sender is performing: 0x0001 for request, 0x0002 for reply.
    _sha :: MacAddress,
    -- ^ Sender hardware address
    _spa :: IPAddress,
    -- ^ Sender protocol address
    _tha :: MacAddress,
    -- ^ Target hardware address
    _tpa :: IPAddress
    -- ^ Target protocol address
  } deriving (Generic, Show, ShowX, NFDataX, BitPack)

-- | ARP's EtherType for multiplexing purposes.
arpEtherType :: BitVector 16
arpEtherType = 0x0806

-- | Construct an IPv4 ARP request.
newArpRequest
  :: MacAddress
  -- ^ Our MAC address
  -> IPAddress
  -- ^ Our IP address
  -> IPAddress
  -- ^ Target IP address
  -> ArpPacket
newArpRequest myMac myIP targetIP
  = ArpPacket {
      _htype = 0x0001,
      _ptype = 0x0800,
      _hlen = 0x06,
      _plen = 0x04,
      _oper = 0x0001,
      _sha = myMac,
      _spa = myIP,
      _tha = broadcastMac,
      _tpa = targetIP
    }
