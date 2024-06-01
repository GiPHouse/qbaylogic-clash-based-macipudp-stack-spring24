{-|
Module      : Clash.Cores.Ethernet.Mac.EthernetTypes
Description : Provides various data types, aliases and constants for the Ethernet protocol.
-}
module Clash.Cores.Ethernet.Mac.EthernetTypes
  ( MacAddress(..)
  , EthernetHeader(..)
  , Preamble
  , broadcastMac
  , preamble
  , startFrameDelimiter
  , toEthernetC
  ) where

import Clash.Prelude

import Protocols
import Protocols.Extra.PacketStream

import Clash.Cores.Ethernet.IP.IPv4Types

import Control.DeepSeq ( NFData )
import Data.Bifunctor qualified as B
import Data.Tuple


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

-- | Convert an IPv4Address to the corresponding EthernetHeader.
toEthernetC :: HiddenClockResetEnable dom
  => Signal dom MacAddress
  -> Circuit (PacketStream dom dataWidth IPv4Address) (PacketStream dom dataWidth EthernetHeader)
toEthernetC macSrc = Circuit (swap . unbundle . helper macSrc . bundle)
  where
    helper :: Signal dom MacAddress
      -> Signal dom (Maybe (PacketStreamM2S dataWidth IPv4Address), PacketStreamS2M)
      -> Signal dom (Maybe (PacketStreamM2S dataWidth EthernetHeader), PacketStreamS2M)
    helper mac pkt = go <$> mac <*> pkt

    go :: MacAddress
      -> (Maybe (PacketStreamM2S dataWidth IPv4Address), PacketStreamS2M)
      -> (Maybe (PacketStreamM2S dataWidth EthernetHeader), PacketStreamS2M)
    go mac = B.first $ fmap $ fmap (toEthernet mac)

    toEthernet :: MacAddress -> IPv4Address -> EthernetHeader
    toEthernet src _ = EthernetHeader {
      _macDst = hardCodedMac,
      _macSrc = src,
      _etherType = 0x0800 -- IPv4 EtherType
    }

hardCodedMac :: MacAddress
hardCodedMac = MacAddress (0x00 :> 0xe0 :> 0x6c :> 0x38 :> 0xd0:> 0x2c :> Nil)

-- | Broadcast MAC address.
broadcastMac :: MacAddress
broadcastMac = MacAddress (repeat 0xFF)
