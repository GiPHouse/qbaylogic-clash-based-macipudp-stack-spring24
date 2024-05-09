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
import Clash.Cores.Ethernet.DownConverter
import Clash.Cores.Ethernet.PacketStream ( PacketStream, PacketStreamM2S(..), fromPacketStream )
import Clash.Cores.Ethernet.RGMII ( RGMIIRXChannel(rgmii_rx_clk), rgmiiReceiver, unsafeRgmiiRxC )
import Clash.Cores.Ethernet.RxStack ( rxStack )

-- import uart
import Clash.Cores.UART ( BaudGenerator )

-- import ECP5
import Clash.Lattice.ECP5.Prims ( delayg, iddrx1f )
import Clash.Lattice.ECP5.UART ( uartTxNoBaudGenC )

-- import protocols
import Clash.Cores.Ethernet.AsyncFIFO
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
  => Clock domEth
  -- ^ Clock to pass on to the RGMII receiver
  -> Reset domEth
  -- ^ Reset to pass on to the RGMII receiver
  -> BaudGenerator dom
  -- ^ Baud generator for the UART sender
  -> RGMIIRXChannel domEth domDDREth
  -- ^ Input channel
  -> Signal dom Bit
  -- ^ Output signal
uartEthRxStack clkEth rstEth baudGen uartTxS  = uartTxBitS
  where
    ckt :: Circuit (RGMIIRXChannel domEth domDDREth) (CSignal dom Bit)
    ckt = exposeClockResetEnable (unsafeRgmiiRxC (delayg d80) iddrx1f) clkEth rstEth enableGen
       |> rxStack @4 (rgmii_rx_clk uartTxS)
       |> asyncFifoC d10 hasClock hasReset hasEnable hasClock hasReset hasEnable
       |> downConverterC
       |> uartTxNoBaudGenC baudGen
    (_, CSignal uartTxBitS) = toSignals ckt (uartTxS, CSignal $ pure ())
