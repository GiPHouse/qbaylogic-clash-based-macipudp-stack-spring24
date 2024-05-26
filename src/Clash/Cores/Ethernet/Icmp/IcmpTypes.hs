{-|
Module      : Clash.Cores.Ethernet.Icmp.ICMPTypes.hs
Description : Provides various data types, aliases and constants for ICMP.
-}
module Clash.Cores.Ethernet.Icmp.IcmpTypes
(
  IcmpHeader(..),
  IcmpHeaderLite(..)
)
where

import Clash.Prelude

import Control.DeepSeq ( NFData )


-- | Full ICMP header
data IcmpHeader = IcmpHeader {
  _type :: BitVector 8,
  _code :: BitVector 8,
  _checksum :: BitVector 16
  } deriving (Show, ShowX, Eq, Generic, BitPack, NFDataX, NFData)

-- | Small ICMP header with only the type
data IcmpHeaderLite = IcmpHeaderLite {
  _typeL :: BitVector 8,
  _checksumL :: BitVector 16
  } deriving (Show, ShowX, Eq, Generic, BitPack, NFDataX, NFData)
