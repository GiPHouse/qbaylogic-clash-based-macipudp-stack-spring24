{-# language RecordWildCards #-}

{-|
Module      : Clash.Cores.Ethernet.IP.IPv4Types
Description : Provides various data types, aliases and constants for IPv4.
-}
module Clash.Cores.Ethernet.IP.IPv4Types
  ( IPv4Address(..)
  , IPv4Header(..)
  , IPv4HeaderLite(..)
  , toLite
  , toLiteC
  , fromLite
  , fromLiteC
  ) where

import Clash.Prelude
import Control.DeepSeq ( NFData )
import Data.Bifunctor qualified as B
import Data.Tuple
import Protocols
import Protocols.Extra.PacketStream

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

toLite :: IPv4Header -> IPv4HeaderLite
toLite IPv4Header {..} = IPv4HeaderLite
  { _ipv4lSource = _ipv4Source
  , _ipv4lDestination = _ipv4Destination
  , _ipv4lPayloadLength = _ipv4Length - 20 -- We do not support IHLs other than 5
  }

-- | Shrinks IPv4 headers
toLiteC :: Circuit (PacketStream dom n IPv4Header) (PacketStream dom n IPv4HeaderLite)
toLiteC = Circuit (swap . unbundle . go . bundle)
  where
    go = fmap $ B.first $ fmap $ fmap toLite

fromLite :: IPv4HeaderLite -> IPv4Header
fromLite header = IPv4Header { _ipv4Version = 4
                             , _ipv4Ihl = ipv4Ihl
                             , _ipv4Dscp = 0
                             , _ipv4Ecn = 0
                             , _ipv4Length = _ipv4lPayloadLength header + zeroExtend (4 * ipv4Ihl)
                             , _ipv4Id = 0
                             , _ipv4FlagReserved = False
                             , _ipv4FlagDF = False
                             , _ipv4FlagMF = False
                             , _ipv4FragmentOffset = 0
                             , _ipv4Ttl = 64
                             , _ipv4Protocol = 0
                             , _ipv4Checksum = 0
                             , _ipv4Source = _ipv4lSource header
                             , _ipv4Destination = _ipv4lDestination header
                             }
  where
    ipv4Ihl = 5

-- | Produce a full IPv4 header from a lite one.
--   Note that this does *not* compute the checksum.
fromLiteC :: Circuit (PacketStream dom n IPv4HeaderLite) (PacketStream dom n IPv4Header)
fromLiteC = Circuit (swap . unbundle . go . bundle)
  where
    go = fmap $ B.first $ fmap $ fmap fromLite
