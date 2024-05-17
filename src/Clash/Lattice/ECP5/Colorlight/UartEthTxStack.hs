{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Module      : Clash.Lattice.ECP5.Colorlight.UartEthTxStack
Description : Complete receive stack from a UART signal to an ethernet tx channel for debugging the transmit stack from the ethernet core
-}
module Clash.Lattice.ECP5.Colorlight.UartEthTxStack
  ( uartEthTxStack
  ) where

import Data.Proxy

import Clash.Cores.Crc.Catalog
import Clash.Cores.Crc.Internal

import Clash.Cores.Ethernet.PacketBuffer (packetBufferC)
import Clash.Cores.Ethernet.PacketStream
import Clash.Cores.Ethernet.RGMII
import Clash.Cores.Ethernet.TxStack
import Clash.Cores.Ethernet.UpConverter
import Clash.Cores.UART (BaudGenerator)
import Clash.Lattice.ECP5.Prims (delayg, oddrx1f)
import Clash.Lattice.ECP5.UART
import Clash.Prelude
import Protocols
import Protocols.Internal (CSignal (CSignal))

$(deriveHardwareCrc (Proxy @Crc32_ethernet) d8 d4)

-- | Runs incoming packets from a UART signal through the ethernet transmit
-- stack, creating an RGMIITXChannel.
uartEthTxStack
  :: forall (dom :: Domain) (domEth :: Domain) (domDDREth :: Domain)
   . ( KnownDomain dom
     , KnownDomain domEth
     , KnownDomain domDDREth
     , HiddenClockResetEnable dom
     , KnownConf domEth
        ~ 'DomainConfiguration domEth 8000 'Rising 'Asynchronous 'Unknown 'ActiveHigh
     , KnownConf domDDREth
        ~ 'DomainConfiguration domDDREth 4000 'Rising 'Asynchronous 'Unknown 'ActiveHigh
     )
  => Clock domEth
  -- ^ Clock to pass on to the RGMII sender
  -> Reset domEth
  -- ^ Reset to pass on to the RGMII sender
  -> BaudGenerator dom
  -- ^ Baud generator for the UART receiver
  -> Signal dom Bit
  -- ^ Input signal
  -> RGMIITXChannel domDDREth
  -- ^ Output channel
uartEthTxStack clkEth rstEth baudGen uartRxS = snd $ toSignals ckt (CSignal uartRxS, pure ())
 where
  ckt =
    uartRxNoBaudGenC' baudGen
      |> unsafeToPacketStream
      |> upConverterC
      |> packetBufferC d10 d6
      |> txStack @4 clkEth rstEth enableGen
      |> exposeClockResetEnable (rgmiiTxC (delayg d0) oddrx1f) clkEth rstEth enableGen
