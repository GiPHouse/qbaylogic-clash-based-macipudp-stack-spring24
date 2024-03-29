{-# language FlexibleContexts #-}

module Clash.Lattice.ECP5.Colorlight.UartEthRxStack
( uartEthRxStack
) where

-- import prelude
import Clash.Prelude

-- import ethernet
import Clash.Cores.Ethernet.PacketStream ( PacketStream, PacketStreamM2S(..) )
import Clash.Cores.Ethernet.RGMII ( RGMIIRXChannel(rgmii_rx_clk), rgmiiReceiver )
import Clash.Cores.Ethernet.RxStack ( rxStack )
import Clash.Cores.Ethernet.DownConverter

-- import uart
import Clash.Cores.UART ( BaudGenerator )

-- import ECP5
import Clash.Lattice.ECP5.Prims ( delayg, iddrx1f )
import Clash.Lattice.ECP5.UART ( uartTxNoBaudGenC )

-- import protocols
import Protocols ( Circuit, toSignals, (|>) )
import Protocols.Internal ( CSignal(CSignal) )
import Clash.Cores.Ethernet.AsyncFIFO

uartEthRxStack
  :: forall dom domEth domDDREth.
  ( KnownDomain dom
  , KnownDomain domEth
  , HiddenClockResetEnable dom
  , KnownDomain domDDREth
  , KnownConf domEth ~ 'DomainConfiguration domEth 8000 'Rising 'Asynchronous 'Unknown 'ActiveHigh
  , KnownConf domDDREth ~ 'DomainConfiguration domDDREth 4000 'Rising 'Asynchronous 'Unknown 'ActiveHigh
  )
  => BaudGenerator dom
  -> RGMIIRXChannel domEth domDDREth
  -> Signal dom Bit
uartEthRxStack baudGen channel = uartTxBitS
  where
    rgmiiRx = rgmiiReceiver channel (delayg d80) iddrx1f
    packetStream = fmap rgmiiRecvToPacketStream rgmiiRx

    ckt :: Circuit (PacketStream domEth 1 ()) (CSignal dom Bit)
    ckt = rxStack @4 (rgmii_rx_clk channel)
           |> asyncFifoC d10 hasClock hasReset hasEnable hasClock hasReset hasEnable
           |> downConverterC
           |> uartTxNoBaudGenC baudGen

    (_, CSignal uartTxBitS) = toSignals ckt (packetStream, CSignal $ pure ())


rgmiiRecvToPacketStream :: (Bool, Maybe (BitVector 8)) -> Maybe (PacketStreamM2S 1 ())
rgmiiRecvToPacketStream (err, Just dat) = Just $ PacketStreamM2S (singleton dat) Nothing () err
rgmiiRecvToPacketStream (_, Nothing) = Nothing

