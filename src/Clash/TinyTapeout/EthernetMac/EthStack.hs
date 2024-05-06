{-# language MultiParamTypeClasses #-}
{-# language FlexibleContexts #-}

module Clash.TinyTapeout.EthernetMac.EthStack where

import Clash.Prelude
import qualified Clash.Explicit.Prelude as E
import Data.Proxy

import Protocols
import qualified Protocols.DfConv as DfConv

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

import Data.Maybe (isNothing, isJust, catMaybes)
import Clash.Cores.Ethernet.Util (toMaybe)
import Prelude (unlines)
import Data.Char (ord, chr)
import qualified Data.List as L

import Data.Bifunctor (bimap)

$(deriveHardwareCrc (Proxy @Crc32_ethernet) d8 d1)

type CreditsBlobWords = 163

creditsBlob :: MemBlob CreditsBlobWords 8
creditsBlob = $(memBlobTH Nothing $ fmap (fromIntegral @Int @(BitVector 8) . ord) $ unlines
  [ "Designed by:"
  , "Jasper Laumen"
  , "Mart Koster"
  , "Daan Weessies"
  , "Cato van Ojen"
  , "Jasmijn Bookelmann"
  , "Tim Wallet"
  , "Bryan Rinders"
  , "Matthijs Muis"
  , "Rowan Goemans"
  , "\nSponsored by QbayLogic"
  ])

creditsC
  :: HiddenClockResetEnable dom
  => Circuit (PacketStream dom 1 EthernetHeader) (PacketStream dom 1 EthernetHeader)
creditsC = fromSignals go
  where
    goT cnt (_, Nothing, bwdIn) = (cnt, (cnt, bwdIn, Nothing))
    goT cnt (bv, Just fwdIn, bwdIn) = (nextCnt, (nextCnt, PacketStreamS2M readyOut, fwdOut))
      where
        lastIn = isJust $ _last fwdIn
        readyIn = _ready bwdIn
        nextCnt = if lastIn && readyIn then satSucc SatWrap cnt else cnt
        readyOut = isNothing (_last fwdIn) || (cnt == maxBound && readyIn)

        fwdOut = toMaybe lastIn $ fwdIn { _data = singleton bv }

    go (fwdIn, bwdIn) = (bwdOut, fwdOut)
      where
        romOut = romBlob creditsBlob addr
        (addr, bwdOut, fwdOut) = mealyB goT (0 :: Index CreditsBlobWords) (romOut, fwdIn, bwdIn)

ethernetEndpoints
  :: HiddenClockResetEnable dom
  => Circuit (PacketStream dom 1 EthernetHeader) (PacketStream dom 1 EthernetHeader)
ethernetEndpoints = circuit $ \eth -> do
  let dispatch = (\h -> 0x2001 == _etherType h) :> (\h -> 0x2002 == _etherType h) :> Nil
  [echoEth, creditsEth] <- packetDispatcherC dispatch -< eth

  credits <- creditsC -< creditsEth

  packetArbiterC RoundRobin -< [echoEth, credits]

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
            |> asyncFifoC d6 clkRx rstRx enRx hasClock hasReset hasEnable
            |> macDepacketizerC
            |> ethernetEndpoints
            |> macPacketizerC
            |> paddingInserterC d60
            |> fcsInserterC
            |> preambleInserterC
            |> interpacketGapInserterC d12
            |> miiTxC dflipflop
