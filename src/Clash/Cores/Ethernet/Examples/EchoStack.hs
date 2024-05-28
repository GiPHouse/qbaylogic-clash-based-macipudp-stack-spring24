{-# language FlexibleContexts #-}
{-# language RecordWildCards #-}

{-|
Module      : Clash.Cores.Ethernet.Examples.EchoStack
Description : Simple Ethernet echo stack.
-}
module Clash.Cores.Ethernet.Examples.EchoStack
  ( echoStackC
  ) where

-- import prelude
import Clash.Prelude

-- import ethernet
import Clash.Cores.Ethernet.Examples.RxStack ( rxStack )
import Clash.Cores.Ethernet.Examples.TxStack ( txStack )
import Clash.Cores.Ethernet.Mac.EthernetTypes ( EthernetHeader(..), MacAddress(..) )

-- import protocols
import Protocols ( Circuit, (|>) )
import Protocols.Extra.PacketStream
import Protocols.Extra.PacketStream.PacketBuffer ( packetBufferC )

import Clash.Cores.Crc ( HardwareCrc )
import Clash.Cores.Crc.Catalog ( Crc32_ethernet )


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
  => HardwareCrc Crc32_ethernet 8 4
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
    ckt = rxStack @4 rxClk rxRst rxEn (pure myMac)
            |> packetBufferC d10 d4
            |> mapMeta swapMac
            |> txStack txClk txRst txEn

