{-# language FlexibleContexts #-}
{-# language RecordWildCards #-}

{-|
Module      : Clash.Cores.Ethernet.EchoStack
Description : Simple Ethernet echo stack
-}
module Clash.Cores.Ethernet.EchoStack
( echoStackC
) where

-- import prelude
import Clash.Prelude

-- import ethernet
import Clash.Cores.Ethernet.EthernetTypes ( EthernetHeader(..), MacAddress(..) )
import Clash.Cores.Ethernet.PacketBuffer ( packetBufferC )
import Clash.Cores.Ethernet.RxStack ( rxStack )
import Clash.Cores.Ethernet.TxStack ( txStack )

-- import protocols
import Clash.Cores.Crc ( HardwareCrc )
import Clash.Cores.Crc.Catalog ( Crc32_ethernet )
import Clash.Cores.Ethernet.PacketStream
import Protocols ( Circuit, (|>) )

myMac :: MacAddress
myMac = MacAddress $ 0x00 :> 0x00 :> 0x00 :> 0xff :> 0xff :> 0xff :> Nil

-- | Processes ethernet frames and echoes them back
echoStackC
  :: forall
       (dom :: Domain)
       (domEthRx :: Domain)
       (domEthTx :: Domain)
   . KnownDomain dom
  => KnownDomain domEthRx
  => KnownDomain domEthTx
  => HardwareCrc Crc32_ethernet 8 2
  => HiddenClockResetEnable dom
  => Clock domEthRx
  -> Reset domEthRx
  -> Enable domEthRx
  -> Clock domEthTx
  -> Reset domEthTx
  -> Enable domEthTx
  -> Circuit (PacketStream domEthRx 1 ()) (PacketStream domEthTx 1 ())
echoStackC rxClk rxRst rxEn txClk txRst txEn = ckt
  where
    swapMac hdr@EthernetHeader {..} = hdr { _macSrc = _macDst, _macDst = _macSrc}
    ckt = rxStack @2 rxClk rxRst rxEn (pure myMac)
            |> packetBufferC d10 d4
            |> mapMeta swapMac
            |> txStack txClk txRst txEn
