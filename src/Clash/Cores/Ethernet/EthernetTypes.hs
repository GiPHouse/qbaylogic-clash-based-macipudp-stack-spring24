{-|
Module      : Clash.Cores.Ethernet.EthernetTypes
Description : Provides various data types, aliases and constants for the Ethernet protocol.
-}
module Clash.Cores.Ethernet.EthernetTypes
  ( MacAddress(..)
  , EthernetHeader(..)
  , Preamble
  , broadcastMac
  , preamble
  , startFrameDelimiter
  , IPv4Header(..)
  , IPv4HeaderLite(..)
  , IPv4Address
  , IcmpHeader(..)
  , IcmpHeaderLite(..)
  ) where

import Clash.Prelude
import Control.DeepSeq ( NFData )

-- | Full ICMP header
data IcmpHeader = IcmpHeader {
  _type :: BitVector 8,
  _code :: BitVector 8,
  _checksum :: BitVector 16,
  _restofheader :: BitVector 32
} deriving (Show, ShowX, Eq, Generic, BitPack, NFDataX, NFData)


-- | Small ICMP header with only the type
newtype IcmpHeaderLite = IcmpHeaderLite {_typeL :: BitVector 8}
  deriving (Show, ShowX, Eq, Generic, BitPack, NFDataX, NFData)


-- | Stores a MAC address, which is always 6 bytes long.
newtype MacAddress = MacAddress (Vec 6 (BitVector 8))
  deriving (Show, ShowX, Eq, Generic, BitPack, NFDataX, NFData)

type IPv4Address = Vec 4 (BitVector 8)

-- | (Almost) full IPv4 header. Does not contain options field.
data IPv4Header = IPv4Header
  { _ipv4Version :: BitVector 4
  , _ipv4Ihl :: Unsigned 4
  , _ipv4Dscp :: BitVector 6
  , _ipv4Ecn :: BitVector 2
  , _ipv4Length :: Unsigned 16
  , _ipv4Id :: BitVector 16
  , _ipv4FlagReserved :: Bool
  , _ipv4FlagDF :: Bool
  , _ipv4FlagMF :: Bool
  , _ipv4FragmentOffset :: BitVector 13
  , _ipv4Ttl :: Unsigned 8
  , _ipv4Protocol :: Unsigned 8
  , _ipv4Checksum :: BitVector 16
  , _ipv4Source :: IPv4Address
  , _ipv4Destination :: IPv4Address
  } deriving (Show, ShowX, Eq, Generic, BitPack, NFDataX, NFData)

-- | Partial IPv4 header.
data IPv4HeaderLite = IPv4HeaderLite
  { _ipv4lSource :: IPv4Address
  , _ipv4lDestination :: IPv4Address
  , _ipv4lPayloadLength :: Unsigned 16
  } deriving (Show, ShowX, Eq, Generic, BitPack, NFDataX, NFData)

-- | Stores a link-layer Ethernet header, that is, a destination MAC address,
--   a source MAC address, and an EtherType.
data EthernetHeader = EthernetHeader {
  _macDst :: MacAddress,
  _macSrc :: MacAddress,
  _etherType :: BitVector 16
} deriving (Show, ShowX, Eq, Generic, BitPack, NFDataX, NFData)

-- | A vector of 8 bytes, which is the size of the Ethernet preamble + start frame delimiter.
type Preamble = Vec 8 (BitVector 8)

-- | The actual preamble, each byte ordered least significant bit first.
preamble :: Preamble
preamble = replicate d7 0x55 :< 0xD5

-- | Ethernet start frame delimiter (SFD), least significant bit first.
startFrameDelimiter :: BitVector 8
startFrameDelimiter = 0xD5

-- | Broadcast MAC address.
broadcastMac :: MacAddress
broadcastMac = MacAddress (repeat 0xFF)
