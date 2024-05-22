{-|
Module      : Clash.Cores.IP.IPv4Types
Description : Provides various data types, aliases and constants for IPv4.
-}
module Clash.Cores.IP.IPv4Types
(
  IPv4Address(..),
  IPv4HeaderLite(..)
)
where

import Clash.Prelude

import Control.DeepSeq ( NFData )


-- | IPv4 address.
newtype IPv4Address = IPv4Address (Vec 4 (BitVector 8))
  deriving (Generic, Show, ShowX, NFDataX, NFData, Eq, BitPack)


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
