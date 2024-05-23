{-# language RecordWildCards #-}

{-|
Module      : Clash.Cores.Ethernet.EthernetTypes
Description : Provides various data types, aliases and constants for the Ethernet protocol.
-}
module Clash.Cores.Ethernet.EthernetTypes
  ( MacAddress(..)
  , EthernetHeader(..)
  , fromLite
  , fromLiteC
  , Preamble
  , broadcastMac
  , preamble
  , startFrameDelimiter
  , toEthernetC
  , IPv4Address
  , IPv4Header(..)
  , IPv4HeaderLite(..)
  , toLite
  , toLiteC
  ) where

import Clash.Prelude

import Clash.Cores.Ethernet.PacketStream
import Clash.Cores.IP.IPv4Types
import Control.DeepSeq ( NFData )
import Data.Bifunctor qualified as B
import Data.Tuple
import Protocols

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
hardCodedMac = MacAddress (0x8C :> 0x8C :> 0xAA :> 0xC8 :> 0x2B :> 0xEE :> Nil)

-- | Broadcast MAC address.
broadcastMac :: MacAddress
broadcastMac = MacAddress (repeat 0xFF)

toLite :: IPv4Header -> IPv4HeaderLite
toLite IPv4Header {..} = IPv4HeaderLite
  { _ipv4lSource = _ipv4Source
  , _ipv4lDestination = _ipv4Destination
  , _ipv4lPayloadLength = _ipv4Length - zeroExtend (4 * _ipv4Ihl)
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
