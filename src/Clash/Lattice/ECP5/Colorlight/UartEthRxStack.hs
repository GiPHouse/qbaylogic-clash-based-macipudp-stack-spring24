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
import Protocols.DfConv ( fifo, registerFwd )
import Protocols.Internal ( CSignal(CSignal) )
import Protocols.DfConv (registerBwd)
import Clash.Cores.Ethernet.AsyncFIFO

-- SNat depth
--   -> Clock wDom
--   -> Reset wDom
--   -> Enable wDom
--   -> Clock rDom
--   -> Reset rDom
--   -> Enable rDom
--   -> Circuit (PacketStream wDom dataWidth metaType) (PacketStream rDom dataWidth metaType)
-- Circuit (PacketStream wDom dataWidth metaType) (PacketStream rDom dataWidth metaType)
-- _ :: Circuit (PacketStream domEth 1 ()) (PacketStream dom 1 ())

{-# NOINLINE uartEthRxStack #-}
uartEthRxStack
  :: forall dom domEth domDDREth.
  ( KnownDomain dom
  , KnownDomain domEth
  , HiddenClockResetEnable dom
  , KnownDomain domDDREth
  , KnownConf domEth ~ 'DomainConfiguration domEth 8000 'Rising 'Synchronous 'Unknown 'ActiveHigh
  , KnownConf domDDREth ~ 'DomainConfiguration domDDREth 4000 'Rising 'Synchronous 'Unknown 'ActiveHigh
  )
  => BaudGenerator dom
  -> RGMIIRXChannel domEth domDDREth
  -> Signal dom Bit
uartEthRxStack baudGen channel = uartTxBitS
  where 
    rgmiiRx = rgmiiReceiver channel (delayg d80) iddrx1f
    packetStream = fmap rgmiiRecvToPacketStream rgmiiRx

    ckt :: Circuit (PacketStream domEth 1 ()) (CSignal dom Bit)
    ckt = rxStack @1 (rgmii_rx_clk channel)
        |> asyncFifoC d4 hasClock resetGen enableGen hasClock hasReset hasEnable
      |> downConverterC
      |> uartTxNoBaudGenC baudGen

    (_, CSignal uartTxBitS) = toSignals ckt (packetStream, CSignal $ pure ())


rgmiiRecvToPacketStream :: (Bool, Maybe (BitVector 8)) -> Maybe (PacketStreamM2S 1 ())
rgmiiRecvToPacketStream (err, Just dat) = Just $ PacketStreamM2S (singleton dat) Nothing () err
rgmiiRecvToPacketStream (_, Nothing) = Nothing

