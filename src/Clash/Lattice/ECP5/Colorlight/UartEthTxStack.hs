{-# language FlexibleContexts #-}

module Clash.Lattice.ECP5.Colorlight.UartEthTxStack
  ( uartEthTxStack
  ) where

import Clash.Cores.Ethernet.PacketBuffer ( packetBufferC )
import Clash.Cores.Ethernet.PacketStream
import Clash.Cores.Ethernet.RGMII
import Clash.Cores.Ethernet.TxStack
import Clash.Cores.Ethernet.UpConverter
import Clash.Cores.UART ( BaudGenerator )
import Clash.Lattice.ECP5.Prims ( delayg, oddrx1f )
import Clash.Lattice.ECP5.UART
import Clash.Prelude
import Protocols
import Protocols.Internal ( CSignal(CSignal) )

uartEthTxStack
  :: forall (dom :: Domain) (domEth :: Domain) (domDDREth :: Domain)
   . ( KnownDomain dom
     , KnownDomain domEth
     , KnownDomain domDDREth
     , HiddenClockResetEnable dom
     , KnownConf domEth ~ 'DomainConfiguration domEth 8000 'Rising 'Asynchronous 'Unknown 'ActiveHigh
     , KnownConf domDDREth ~ 'DomainConfiguration domDDREth 4000 'Rising 'Asynchronous 'Unknown 'ActiveHigh)
  => Clock domEth
  -> Reset domEth
  -> BaudGenerator dom
  -> Signal dom Bit
  -> RGMIITXChannel domDDREth
uartEthTxStack clkEth rstEth baudGen uartRxS = snd $ toSignals ckt (CSignal uartRxS, pure ())
  where
    ckt = uartRxNoBaudGenC' baudGen
          |> unsafeToPacketStream
          |> upConverterC
          |> packetBufferC d10
          |> txStack @4 clkEth rstEth enableGen
          |> exposeClockResetEnable (rgmiiTxC (delayg d0) oddrx1f) clkEth rstEth enableGen
