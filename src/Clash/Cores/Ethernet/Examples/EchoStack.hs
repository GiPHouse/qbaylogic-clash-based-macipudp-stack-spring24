{-# language FlexibleContexts #-}
{-# language RecordWildCards #-}

{-|
Module      : Clash.Cores.Ethernet.Examples.EchoStack
Description : Simple Ethernet echo stack.
-}
module Clash.Cores.Ethernet.Examples.EchoStack
  ( ipEchoStackC
  ) where

-- import prelude
import Clash.Prelude

-- import ethernet
import Clash.Cores.Ethernet.Examples.RxStacks ( ipRxStack )
import Clash.Cores.Ethernet.Examples.TxStacks ( ipTxStack )
import Clash.Cores.Ethernet.Mac.EthernetTypes ( MacAddress(..) )

-- import protocols
import Protocols
import Protocols.Extra.PacketStream
import Protocols.Extra.PacketStream.PacketBuffer ( packetBufferC )

import Clash.Cores.Crc ( HardwareCrc )
import Clash.Cores.Crc.Catalog ( Crc32_ethernet )

import Clash.Cores.Ethernet.IP.IPv4Types

-- | Processes IP packets and echoes them back
ipEchoStackC
  :: forall
       (dom :: Domain)
       (domEthRx :: Domain)
       (domEthTx :: Domain)
   . KnownDomain dom
  => KnownDomain domEthRx
  => KnownDomain domEthTx
  => HardwareCrc Crc32_ethernet 8 4
  => HiddenClockResetEnable dom
  => Clock domEthRx
  -> Reset domEthRx
  -> Enable domEthRx
  -> Clock domEthTx
  -> Reset domEthTx
  -> Enable domEthTx
  -> Signal dom MacAddress
  -> Signal dom (IPv4Address, IPv4Address)
  -> Circuit (PacketStream domEthRx 1 ()) (PacketStream domEthTx 1 ())
ipEchoStackC rxClk rxRst rxEn txClk txRst txEn mac ip = ckt
  where
    swapIp hdr@IPv4HeaderLite {..} = hdr { _ipv4lSource = _ipv4lDestination, _ipv4lDestination = _ipv4lSource}
    ckt = ipRxStack @4 rxClk rxRst rxEn mac ip
            |> packetBufferC d10 d4
            |> mapMeta swapIp
            |> ipTxStack @4 txClk txRst txEn mac
