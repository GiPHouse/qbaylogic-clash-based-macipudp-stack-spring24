{-# language FlexibleContexts #-}

module Clash.Lattice.ECP5.Colorlight.UartEthRxStack
( uartEthRxStack
) where

-- import prelude
import Clash.Prelude

-- import maybe, proxy
import Data.Maybe ( fromJust )
import Data.Proxy ( Proxy(Proxy) )

-- import ethernet
import Clash.Cores.Ethernet.PacketStream ( PacketStream, PacketStreamM2S(..) )
import Clash.Cores.Ethernet.RGMII ( RGMIIRXChannel, rgmiiReceiver )
import Clash.Cores.Ethernet.RxStack ( rxStack )
import Clash.Cores.Ethernet.Util ( extractSignal )

-- import uart
import Clash.Cores.UART ( ValidBaud, baudGenerator )

-- import ECP5
import Clash.Lattice.ECP5.Prims ( delayg, iddrx1f )
import Clash.Lattice.ECP5.UART ( uartTxNoBaudGenC )

-- import protocols
import Protocols ( Circuit, toSignals, (|>) )
import Protocols.DfConv ( fifo )
import Protocols.Internal ( CSignal(CSignal) )


uartEthRxStack
  :: forall baud dom domEth0 domDDREth0.
  ( KnownDomain domDDREth0
  , HiddenClockResetEnable domEth0
  , ValidBaud dom baud
  , KnownConf domEth0 ~ 'DomainConfiguration domEth0 8000 'Rising 'Asynchronous 'Unknown 'ActiveHigh
  , KnownConf domDDREth0 ~ 'DomainConfiguration domDDREth0 4000 'Rising 'Asynchronous 'Unknown 'ActiveHigh
  , dom ~ domEth0 )
  => SNat baud
  -> "eth" ::: RGMIIRXChannel domEth0 domDDREth0
  -> "uart_tx" ::: Signal dom Bit
uartEthRxStack baud channel = extractSignal uartTxBit
  where
    rgmiiRx = rgmiiReceiver channel (delayg d80) iddrx1f
    packetStream = fmap rgmiiRecvToPacketStream rgmiiRx

    ckt :: Circuit (PacketStream domEth0 1 ()) (CSignal dom Bit)
    ckt = rxStack
      |> fifo Proxy Proxy (SNat @16000)
      |> uartTxNoBaudGenC (baudGenerator baud)

    (_, uartTxBit) = toSignals ckt (packetStream, CSignal $ pure ())


-- uartEthRxStack :: RGMIIRXChannel DomEth1 DomDDREth1 -> Signal dom Bit
-- uartEthRxStack channel = rgmiiReceiver channel _ _ |> DfConv.fifo Proxy Proxy (P.SNat @16000) |> uartTxNoBaudGenC

rgmiiRecvToPacketStream :: (Bool, Maybe (BitVector 8)) -> Maybe (PacketStreamM2S 1 ())
rgmiiRecvToPacketStream input =
    let (err, dat) = input
        dat' = fromJust dat
    in case dat of
  Nothing -> Nothing
  _       -> Just $ PacketStreamM2S {_data = pure dat', _last = Nothing, _meta = (), _abort = err}


