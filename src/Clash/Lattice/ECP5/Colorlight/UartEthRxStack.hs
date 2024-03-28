{-# language FlexibleContexts #-}

module Clash.Lattice.ECP5.Colorlight.UartEthRxStack
( uartEthRxStack
) where

-- import prelude
import Clash.Prelude

-- import maybe, proxy
import Data.Proxy ( Proxy(Proxy) )

-- import ethernet
import Clash.Cores.Ethernet.PacketStream ( PacketStream, PacketStreamM2S(..) )
import Clash.Cores.Ethernet.RGMII ( RGMIIRXChannel, rgmiiReceiver )
import Clash.Cores.Ethernet.RxStack ( rxStack )

-- import uart
import Clash.Cores.UART ( ValidBaud )

-- import ECP5
import Clash.Lattice.ECP5.Prims ( delayg, iddrx1f )
import Clash.Lattice.ECP5.UART ( uartTxC )

-- import protocols
import Protocols ( Circuit, toSignals, (|>) )
import Protocols.DfConv ( fifo )
import Protocols.Internal ( CSignal(CSignal) )


uartEthRxStack
  :: forall baud domEth domDDREth.
  ( KnownDomain domDDREth
  , HiddenClockResetEnable domEth
  , ValidBaud domEth baud
  , KnownConf domEth ~ 'DomainConfiguration domEth 8000 'Rising 'Asynchronous 'Unknown 'ActiveHigh
  , KnownConf domDDREth ~ 'DomainConfiguration domDDREth 4000 'Rising 'Asynchronous 'Unknown 'ActiveHigh
  )
  => SNat baud
  -> RGMIIRXChannel domEth domDDREth
  -> Signal domEth Bit
uartEthRxStack baud channel = uartTxBitS
  where
    rgmiiRx = rgmiiReceiver channel (delayg d80) iddrx1f
    packetStream = fmap rgmiiRecvToPacketStream rgmiiRx

    ckt :: Circuit (PacketStream domEth 1 ()) (CSignal domEth Bit)
    ckt = rxStack
      |> fifo Proxy Proxy (SNat @16000)
      |> uartTxC baud

    (_, CSignal uartTxBitS) = toSignals ckt (packetStream, CSignal $ pure ())

rgmiiRecvToPacketStream :: (Bool, Maybe (BitVector 8)) -> Maybe (PacketStreamM2S 1 ())
rgmiiRecvToPacketStream (err, Just dat) = Just $ PacketStreamM2S (singleton dat) Nothing () err
rgmiiRecvToPacketStream (_, Nothing) = Nothing

