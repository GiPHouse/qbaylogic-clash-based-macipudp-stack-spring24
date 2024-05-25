{-# language MultiParamTypeClasses #-}
{-# language FlexibleContexts #-}

module Clash.TinyTapeout.EthernetMac.EthStack where

import Clash.Prelude
import qualified Clash.Explicit.Prelude as E
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
import Clash.Cores.Ethernet.PacketArbiter
import Clash.Cores.Ethernet.PacketDispatcher
import Clash.Cores.Ethernet.AsyncFIFO
import Clash.Cores.Ethernet.DownConverter
import Clash.Cores.Ethernet.UpConverter

import Clash.TinyTapeout.EthernetMac.Credits
import Clash.TinyTapeout.EthernetMac.IcmpEcho

import Data.Maybe (isNothing, isJust)
import Clash.Cores.Ethernet.Util (toMaybe)

$(deriveHardwareCrc (Proxy @Crc32_ethernet) d8 d2)

type CreditsBlobWords = 98
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

stack
  :: HiddenClockResetEnable domTx
  => KnownDomain domRx
  => Clock domRx
  -> Reset domRx
  -> Circuit (MIIRXChannel domRx) (MIITXChannel domTx)
stack clkRx rstRx = ckt
  where
    enRx = enableGen
    ckt = (exposeClockResetEnable (unsafeMiiRxC (E.dflipflop clkRx)) clkRx rstRx enRx)
            |> (exposeClockResetEnable preambleStripperC clkRx rstRx enRx)
            -- |> fcsValidator
            |> (exposeClockResetEnable (upConverterC @2) clkRx rstRx enRx)
            |> asyncFifoC d4 clkRx rstRx enRx hasClock hasReset hasEnable
            |> macDepacketizerC
            |> ethernetEndpoints
            |> macPacketizerC
            |> paddingInserterC d60
            |> fcsInserterC
            |> preambleInserterC
            |> downConverterC
            |> interpacketGapInserterC d12
            |> miiTxC dflipflop
