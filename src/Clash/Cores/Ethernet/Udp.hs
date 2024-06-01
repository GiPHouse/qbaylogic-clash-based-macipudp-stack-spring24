{-# language RecordWildCards #-}

{-|
Module      : Clash.Cores.Ethernet.Udp
Description : Udp
-}
module Clash.Cores.Ethernet.Udp where

import Clash.Prelude
import Clash.Cores.Ethernet.IP.IPv4Types
import Protocols
import Protocols.Extra.PacketStream
import Protocols.Extra.PacketStream.Packetizers (packetizerC, depacketizerC)

-- | The full UDP header
data UdpHeader = UdpHeader
  { _udpSrcPort :: Unsigned 16
  , _udpDstPort :: Unsigned 16
  , _udpLength :: Unsigned 16
  , _udpChecksum :: Unsigned 16
  } deriving (Generic, NFDataX, BitPack, Eq, Show, ShowX)

data UdpHeaderLite = UdpHeaderLite
  { _udplSrcPort :: Unsigned 16
  , _udplDstPort :: Unsigned 16
  , _udplLength :: Unsigned 16
  } deriving (Generic, NFDataX, BitPack, Eq, Show, ShowX)

fromUdpLite :: UdpHeaderLite -> UdpHeader
fromUdpLite UdpHeaderLite{..} = UdpHeader
  { _udpSrcPort = _udplSrcPort
  , _udpDstPort = _udplDstPort
  , _udpLength = _udplLength
  , _udpChecksum = 0
  }

udpDepacketizerC
  :: HiddenClockResetEnable dom
  => KnownNat n
  => 1 <= n
  => Circuit (PacketStream dom n IPv4HeaderLite) (PacketStream dom n (IPv4Address, UdpHeaderLite))
udpDepacketizerC = depacketizerC (\udph ipv4lh -> (_ipv4lSource ipv4lh, udph))

udpPacketizerC
  :: HiddenClockResetEnable dom
  => KnownNat n
  => 1 <= n
  => Signal dom IPv4Address
  -> Circuit
      (PacketStream dom n (IPv4Address, UdpHeaderLite))
      (PacketStream dom n IPv4HeaderLite)
udpPacketizerC myIp = mapMetaS (toIp <$> myIp) |> packetizerC fst (fromUdpLite . snd)
  where
    toIp srcIp (dstIp, udpLite) = (ipLite, udpLite)
      where
       ipLite = IPv4HeaderLite
                  { _ipv4lSource = srcIp
                  , _ipv4lDestination = dstIp
                  , _ipv4lProtocol = 0x0011
                  , _ipv4lPayloadLength = _udplLength udpLite + 8
                  } 
    