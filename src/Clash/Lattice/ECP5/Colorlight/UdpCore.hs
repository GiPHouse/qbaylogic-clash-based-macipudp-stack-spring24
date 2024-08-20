{-# language FlexibleContexts #-}
{-# language MultiParamTypeClasses #-}
{-# language NumericUnderscores #-}
{-# language RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Module      : Clash.Lattice.ECP5.Colorlight.UdpCore
Description : Contains the top entity for a UDP core
-}
module Clash.Lattice.ECP5.Colorlight.UdpCore
  ( udpCore
  ) where

import Data.Maybe (isJust)
import Data.Maybe.Extra (toMaybe)

import Clash.Annotations.TH

import Clash.Explicit.Prelude
import Clash.Prelude ( exposeClockResetEnable )

import Clash.Cores.Crc ( deriveHardwareCrc )
import Clash.Cores.Crc.Catalog ( Crc32_ethernet )
import Clash.Lattice.ECP5.Prims
import Clash.Lattice.ECP5.RGMII ( RGMIIRXChannel(..), RGMIITXChannel(..), rgmiiTxC, unsafeRgmiiRxC )


import Protocols ( Circuit, toSignals )
import Protocols.Extra.PacketStream (PacketStream, PacketStreamM2S(..), PacketStreamS2M(..))

import Clash.Cores.Ethernet.Examples.UdpCoreStack ( fullStackC )
import Clash.Cores.Ethernet.Mac.EthernetTypes (MacAddress(..))
import Clash.Cores.Ethernet.IP.IPv4Types (IPv4Address(..))
import Clash.Cores.Ethernet.Udp (UdpHeaderLite(..))
import Data.Proxy ( Proxy(Proxy) )

createDomain vSystem
  { vName="Dom50"
  , vPeriod=20000
  , vActiveEdge=Rising
  , vResetKind=Asynchronous
  , vInitBehavior=Unknown
  , vResetPolarity=ActiveHigh
  }

createDomain vSystem
  { vName="DomEthRx"
  , vPeriod=8000
  , vActiveEdge=Rising
  , vResetKind=Asynchronous
  , vInitBehavior=Unknown
  , vResetPolarity=ActiveHigh
  }

createDomain vSystem
  { vName="DomDDREth"
  , vPeriod=4000
  , vActiveEdge=Rising
  , vResetKind=Asynchronous
  , vInitBehavior=Unknown
  , vResetPolarity=ActiveHigh
  }

createDomain vSystem
  { vName="DomEthTx"
  , vPeriod=8000
  , vActiveEdge=Rising
  , vResetKind=Asynchronous
  , vInitBehavior=Unknown
  , vResetPolarity=ActiveHigh
  }

$(deriveHardwareCrc (Proxy @Crc32_ethernet) d8 d4)

data UDPStreamFwd = UDPStreamFwd
  { _udpValid :: "valid" ::: Bool
  , _udpData :: "data" ::: BitVector 32
  , _udpLast :: "last" ::: BitVector 2
  , _udpLastValid :: "last_valid" ::: Bool
  , _udpAbort :: "abort" ::: Bool
  , _udpIp :: "ip" ::: BitVector 32
  , _udpSrcPort :: "src_port" ::: Unsigned 16
  , _udpDstPort :: "dst_port" ::: Unsigned 16
  , _udpLength :: "length" ::: Unsigned 16
  } deriving (Generic, NFDataX, BitPack, Eq, Show, ShowX)

udpStreamToPacketStream :: UDPStreamFwd -> Maybe (PacketStreamM2S 4 (IPv4Address, UdpHeaderLite))
udpStreamToPacketStream UDPStreamFwd{..} = toMaybe _udpValid pkt
  where
    pkt = PacketStreamM2S
            (unpack _udpData)
            (toMaybe _udpLastValid $ unpack _udpLast)
            (unpack _udpIp, UdpHeaderLite _udpSrcPort _udpDstPort _udpLength)
            _udpAbort

packetStreamToUdpStream :: Maybe (PacketStreamM2S 4 (IPv4Address, UdpHeaderLite)) -> UDPStreamFwd
packetStreamToUdpStream pktM = udpStream
  where
    PacketStreamM2S{..} = fromJustX pktM
    udpStream = UDPStreamFwd
                  { _udpValid = isJust pktM
                  , _udpData = pack _data
                  , _udpLast = pack $ fromJustX _last
                  , _udpLastValid = isJust _last
                  , _udpAbort = _abort
                  , _udpIp = pack $ fst _meta
                  , _udpSrcPort = _udplSrcPort $ snd _meta
                  , _udpDstPort = _udplDstPort $ snd _meta
                  , _udpLength = _udplPayloadLength $ snd _meta
                  }

withPhy
  :: Circuit a (PacketStream domRx 1 ())
  -- ^ phy receiver
  -> Circuit (PacketStream domTx 1 ()) b
  -- ^ phy transmitter
  -> Circuit (c, PacketStream domRx 1 ()) (d, PacketStream domTx 1 ())
  -> Circuit (c, a) (d, b)
withPhy rxPhy txPhy packetStreamHandler = circuit $ \(c, a) -> do
  packetsIn <- rxPhy -< a
  (d, packetsOut) <- packetStreamHandler -< (c, packetsIn)
  b <- txPhy -< packetsOut
  idC -< (d, b)

udpCore
  :: "clk50" ::: Clock Dom50
  -> "rst50" ::: Reset Dom50
  -> "eth_tx_clk_in" ::: Clock DomEthTx
  -> "eth_tx_rst" ::: Reset DomEthTx
  -> "eth" ::: RGMIIRXChannel DomEthRx DomDDREth
  -> "mac" ::: Signal Dom50 (BitVector 48)
  -> "ip" ::: Signal Dom50 (BitVector 32)
  -> "subnetmask" ::: Signal Dom50 (BitVector 32)
  -> "udp_in_fwd" ::: Signal Dom50 UDPStreamFwd
  -> "udp_out_ready" ::: Signal Dom50 Bool
  -> ( "eth" ::: RGMIITXChannel DomDDREth
     , "udp_out_fwd" ::: Signal Dom50 UDPStreamFwd
     , "udp_in_ready" ::: Signal Dom50 Bool
     )
udpCore clk50 rst50 ethTxClk ethTxRst ethRx ourMacBV ourIPv4BV ourSubnetBV udpInFwd udpOutBwd = (ethTx, udpOutFwd', udpInBwd')
  where
    en50 = enableGen @Dom50
    ethRxClk = rgmii_rx_clk ethRx
    ethRxRst = resetGen @DomEthRx
    ethRxEn = enableGen @DomEthRx
    ethTxEn = enableGen @DomEthTx

    ourMac :: Signal Dom50 MacAddress
    ourMac = unpack <$> ourMacBV
    ipSubnet :: Signal Dom50 (IPv4Address, IPv4Address)
    ipSubnet = bundle (unpack <$> ourIPv4BV, unpack <$> ourSubnetBV)


    udpInFwd' = udpStreamToPacketStream <$> udpInFwd
    udpOutBwd' = PacketStreamS2M <$> udpOutBwd

    phyStack = withPhy
      (exposeClockResetEnable (unsafeRgmiiRxC @DomEthRx @DomDDREth (delayg d80) iddrx1f) ethRxClk ethRxRst ethRxEn)
      (exposeClockResetEnable (rgmiiTxC @DomEthTx @DomDDREth (delayg d0) oddrx1f) ethTxClk ethTxRst ethTxEn)
      (exposeClockResetEnable (fullStackC @4 ethRxClk ethRxRst ethRxEn ethTxClk ethTxRst ethTxEn ourMac ipSubnet) clk50 rst50 en50)

    -- Wire up phy signals and upd signals
    ((udpInBwd, _), (udpOutFwd, ethTx))
      = toSignals phyStack ((udpInFwd', ethRx), (udpOutBwd', pure ()))

    udpOutFwd' = packetStreamToUdpStream <$> udpOutFwd
    udpInBwd' = _ready <$> udpInBwd


makeTopEntity 'udpCore
