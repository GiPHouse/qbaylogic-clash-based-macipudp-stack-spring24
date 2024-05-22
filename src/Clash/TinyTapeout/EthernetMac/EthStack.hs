{-# language MultiParamTypeClasses #-}
{-# language FlexibleContexts #-}
{-# language RecordWildCards #-}

module Clash.TinyTapeout.EthernetMac.EthStack where

import Clash.Prelude
import qualified Clash.Explicit.Prelude as E
import Data.Proxy

import Protocols

import Clash.Cores.Crc (HardwareCrc)
import Clash.Cores.Crc.Catalog (Crc32_ethernet)

import Clash.Cores.Ethernet.MII
import Clash.Cores.Ethernet.PacketStream
import Clash.Cores.Ethernet.EthernetTypes
import Clash.Cores.Ethernet.PreambleStripper
import Clash.Cores.Ethernet.InterpacketGapInserter
import Clash.Cores.Ethernet.FrameCheckSequence
import Clash.Cores.Ethernet.PaddingInserter
import Clash.Cores.Ethernet.PreambleInserter
import Clash.Cores.Ethernet.MacDepacketizer
import Clash.Cores.Ethernet.MacPacketizer
import Clash.Cores.Ethernet.PacketArbiter
import Clash.Cores.Ethernet.PacketDispatcher
import Clash.Cores.Ethernet.AsyncFIFO
import Clash.Cores.Ethernet.DownConverter
import Clash.Cores.Ethernet.UpConverter
import Clash.Cores.Ethernet.RxStack
import Clash.Cores.Ethernet.TxStack

import Clash.TinyTapeout.EthernetMac.Credits
import Clash.TinyTapeout.EthernetMac.IcmpEcho

import Data.Maybe (isNothing, isJust)
import Clash.Cores.Ethernet.Util (toMaybe)

-- $(deriveHardwareCrc (Proxy @Crc32_ethernet) d8 d2)

type CreditsBlobWords = 114
creditsBlob :: MemBlob CreditsBlobWords 16
creditsBlob = $(memBlobTH Nothing $ creditsBV d2)

creditsC
  :: HiddenClockResetEnable dom
  => Circuit (PacketStream dom 2 EthernetHeader) (PacketStream dom 2 EthernetHeader)
creditsC = fromSignals go
  where
    goT cnt (_, Nothing, bwdIn) = (cnt, (cnt, bwdIn, Nothing))
    goT cnt (bv, Just fwdIn, bwdIn) = (nextCnt, (nextCnt, PacketStreamS2M readyOut, fwdOut))
      where
        lastWord = cnt == maxBound

        lastIn = isJust $ _last fwdIn
        readyIn = _ready bwdIn
        nextCnt = if lastIn && readyIn then satSucc SatWrap cnt else cnt
        readyOut = isNothing (_last fwdIn) || (lastWord && readyIn)

        fwdOut = toMaybe lastIn $ fwdIn { _data = unpack bv, _last = toMaybe lastWord maxBound }

    go (fwdIn, bwdIn) = (bwdOut, fwdOut)
      where
        romOut = romBlob creditsBlob addr
        (addr, bwdOut, fwdOut) = mealyB goT (0 :: Index CreditsBlobWords) (romOut, fwdIn, bwdIn)

ethernetEndpoints
  :: HiddenClockResetEnable dom
  => Circuit (PacketStream dom 2 EthernetHeader) (PacketStream dom 2 EthernetHeader)
ethernetEndpoints = circuit $ \eth -> do
  let withEthType typ hdr = typ == _etherType hdr

  [echoEth, creditsEth, icmpEchoEth] <-
    packetDispatcherC (withEthType 0x2001 :> withEthType 0x2002 :>
                       withEthType 0x0800 :> Nil) -< eth

  credits <- creditsC -< creditsEth

  icmpEcho <- icmpEchoC -< icmpEchoEth

  packetArbiterC RoundRobin -< [echoEth, credits, icmpEcho]

broadcastMacAddress :: MacAddress
broadcastMacAddress = MacAddress $ repeat 0xFF

myMacAddress :: MacAddress
myMacAddress = MacAddress $ 0x5E :> 0xA4 :> 0x4E :> 0xF4 :> 0x21 :> 0x06 :> Nil

stack
  :: HiddenClockResetEnable domTx
  => KnownDomain domRx
  => HardwareCrc Crc32_ethernet 8 2
  => Clock domRx
  -> Reset domRx
  -> Enable domRx
  -> Circuit (PacketStream domRx 1 ()) (PacketStream domTx 1 ())
stack clkRx rstRx enRx = ckt
  where
    meOrBroadcast m = m == myMacAddress || m == broadcastMacAddress
    swapMac hdr@EthernetHeader {..} = hdr { _macSrc = myMacAddress, _macDst = _macSrc}
    ckt = (exposeClockResetEnable preambleStripperC clkRx rstRx enRx)
            |> (exposeClockResetEnable (upConverterC @2) clkRx rstRx enRx)
            |> asyncFifoC d4 clkRx rstRx enRx hasClock hasReset hasEnable
            |> fcsValidatorC
            |> macDepacketizerC
            |> filterMeta (meOrBroadcast . _macDst)
            |> ethernetEndpoints
            |> mapMeta swapMac
            |> macPacketizerC
            |> paddingInserterC d60
            |> fcsInserterC
            |> preambleInserterC
            |> downConverterC
            |> interpacketGapInserterC d12

miiStack
  :: HiddenClockResetEnable domTx
  => KnownDomain domRx
  => HardwareCrc Crc32_ethernet 8 2
  => Clock domRx
  -> Reset domRx
  -> Circuit (MIIRXChannel domRx) (MIITXChannel domTx)
miiStack clkRx rstRx = ckt
  where
    enRx = enableGen
    ckt = (exposeClockResetEnable (unsafeMiiRxC (E.dflipflop clkRx)) clkRx rstRx enRx)
            |> stack clkRx rstRx enRx
            |> miiTxC dflipflop


stackSeperateDoms
  :: forall
       (dom :: Domain)
       (domEthRx :: Domain)
       (domEthTx :: Domain)
   . KnownDomain dom
  => KnownDomain domEthRx
  => KnownDomain domEthTx
  => HiddenClockResetEnable dom
  => HardwareCrc Crc32_ethernet 8 2
  => Clock domEthRx
  -> Reset domEthRx
  -> Enable domEthRx
  -> Clock domEthTx
  -> Reset domEthTx
  -> Enable domEthTx
  -> Circuit (PacketStream domEthRx 1 ()) (PacketStream domEthTx 1 ())
stackSeperateDoms rxClk rxRst rxEn txClk txRst txEn = ckt
  where
    meOrBroadcast m = m == myMacAddress || m == broadcastMacAddress
    swapMac hdr@EthernetHeader {..} = hdr { _macSrc = myMacAddress, _macDst = _macSrc}
    ckt = rxStack @2 rxClk rxRst rxEn (pure myMacAddress)
            |> filterMeta (meOrBroadcast . _macDst)
            |> ethernetEndpoints
            |> mapMeta swapMac
            |> txStack txClk txRst txEn
