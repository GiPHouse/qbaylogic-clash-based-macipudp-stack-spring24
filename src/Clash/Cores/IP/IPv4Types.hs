{-|
Module      : Clash.Cores.IP.IPv4Types
Description : Provides various data types, aliases and constants for IPv4.
-}
module Clash.Cores.IP.IPv4Types where

import Clash.Prelude

import Control.DeepSeq ( NFData )


newtype IPAddress = IPAddress (Vec 4 (BitVector 8))
  deriving (Generic, Show, ShowX, NFDataX, NFData, Eq, BitPack)
