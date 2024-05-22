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
  , IPv4Address
  , toEthernetC
  , toEthernet
  ) where

import Clash.Prelude

import Protocols

import Clash.Cores.Ethernet.PacketStream
import Clash.Cores.IP.IPv4Types
import Control.DeepSeq ( NFData )
import Data.Bifunctor qualified as B
import Data.Tuple ( swap )


-- | Stores a MAC address, which is always 6 bytes long.
newtype MacAddress = MacAddress (Vec 6 (BitVector 8))
  deriving (Show, ShowX, Eq, Generic, BitPack, NFDataX, NFData)

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


-- | Convert an IPv4Address to the corresponding EthernetHeader.
toEthernetC :: HiddenClockResetEnable dom
  => Signal dom MacAddress
  -> Circuit (PacketStream dom n IPv4Address) (PacketStream dom n EthernetHeader)
toEthernetC macSrc = Circuit (swap . unbundle . helper macSrc . bundle)
  where
    helper mac pkt = go <$> mac <*> pkt
    go mac = B.first $ fmap $ fmap (toEthernet mac)


toEthernet :: MacAddress -> IPv4Address -> EthernetHeader
toEthernet macSrc _ = EthernetHeader {
  _macDst = MacAddress (repeat 0x37),
  _macSrc = macSrc,
  _etherType = 0x0800 -- IPv4 EtherType
}
