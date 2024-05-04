{-# language MultiParamTypeClasses #-}

module Clash.TinyTapeout.EthernetMac.EthStack where

import Clash.Prelude
import Data.Proxy

import Protocols

import Clash.Cores.Crc (deriveHardwareCrc)
import Clash.Cores.Crc.Catalog (Crc32_ethernet)

import Clash.Cores.Ethernet.MII
import Clash.Cores.Ethernet.PacketStream
import Clash.Cores.Ethernet.EthernetTypes
import Clash.Cores.Ethernet.PreambleStripper
import Clash.Cores.Ethernet.InterpacketGapInserter
import Clash.Cores.Ethernet.FcsInserter
import Clash.Cores.Ethernet.PaddingInserter
import Clash.Cores.Ethernet.PreambleInserter
import Clash.Cores.Ethernet.MacDepacketizer
import Clash.Cores.Ethernet.MacPacketizer


$(deriveHardwareCrc (Proxy @Crc32_ethernet) d8 d1)

echoMac
  :: HiddenClockResetEnable dom
  => Circuit (PacketStream dom 1 EthernetHeader) (PacketStream dom 1 EthernetHeader)
echoMac = fromSignals go
  where
    go (fwdIn, bwdIn) = (bwdIn, fwdOut)
      where
        echo h = EthernetHeader
                   { _macSrc = _macDst h
                   , _macDst = _macSrc h
                   , _etherType = _etherType h
                   }
        fwdOut = fmap (\p -> p { _meta = echo $ _meta p }) <$> fwdIn

stack
  :: HiddenClockResetEnable dom
  => Circuit (MIIRXChannel dom) (MIITXChannel dom)
stack = ckt
  where
    ckt = unsafeMiiRxC id
            |> preambleStripperC
            -- TODO: FcsValidator
            |> macDepacketizerC
            |> echoMac
            |> macPacketizerC
            |> paddingInserterC d60
            |> fcsInserterC
            |> preambleInserterC
            |> interpacketGapInserterC d12
            |> miiTxC id
