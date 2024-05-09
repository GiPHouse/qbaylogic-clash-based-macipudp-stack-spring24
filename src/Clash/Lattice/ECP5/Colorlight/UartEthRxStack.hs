{-# language FlexibleContexts #-}

{-|
Module      : Clash.Lattice.ECP5.Colorlight.UartEthRxStack
Description : Complete transmit stack from an ethernet tx channel to a UART signal for debugging the receive stack from the ethernet core
-}
module Clash.Lattice.ECP5.Colorlight.UartEthRxStack
( uartEthRxStack
) where

-- import prelude
import Clash.Prelude

-- import ethernet
import Clash.Cores.Ethernet.DownConverter ( downConverterC )
import Clash.Cores.Ethernet.RGMII ( RGMIIRXChannel(rgmii_rx_clk), unsafeRgmiiRxC )
import Clash.Cores.Ethernet.RxStack ( rxStack )

-- import uart
import Clash.Cores.UART ( BaudGenerator )

-- import ECP5
import Clash.Lattice.ECP5.Prims ( delayg, iddrx1f )
import Clash.Lattice.ECP5.UART ( uartTxNoBaudGenC )

-- import protocols
import Clash.Cores.Ethernet.AsyncFIFO ( asyncFifoC )
import Protocols ( Circuit, toSignals, (|>) )
import Protocols.Internal ( CSignal(CSignal) )

-- | Processes ethernet frames and turns it into a UART signal
uartEthRxStack
  :: forall (dom :: Domain) (domEth :: Domain) (domDDREth :: Domain)
   . KnownDomain dom
  => KnownDomain domEth
  => KnownDomain domDDREth
  => HiddenClockResetEnable dom
  => KnownConf domEth    ~ 'DomainConfiguration domEth    8000 'Rising 'Asynchronous 'Unknown 'ActiveHigh
  => KnownConf domDDREth ~ 'DomainConfiguration domDDREth 4000 'Rising 'Asynchronous 'Unknown 'ActiveHigh
  => BaudGenerator dom
  -- ^ Baud generator for the UART sender
  -> RGMIIRXChannel domEth domDDREth
  -- ^ Input channel
  -> Signal dom Bit
  -- ^ Output signal
uartEthRxStack baudGen ethRxChannel  = uartTxBitS
  where
    ckt :: Circuit (RGMIIRXChannel domEth domDDREth) (CSignal dom Bit)
    ckt = exposeClockResetEnable (unsafeRgmiiRxC (delayg d80) iddrx1f) (rgmii_rx_clk ethRxChannel) resetGen enableGen
       |> rxStack @4 (rgmii_rx_clk ethRxChannel)
       |> asyncFifoC d10 hasClock hasReset hasEnable hasClock hasReset hasEnable
       |> downConverterC
       |> uartTxNoBaudGenC baudGen
    (_, CSignal uartTxBitS) = toSignals ckt (ethRxChannel, CSignal $ pure ())
