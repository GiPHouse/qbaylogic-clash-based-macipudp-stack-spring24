{-# LANGUAGE FlexibleContexts #-}

module Clash.Lattice.ECP5.Colorlight.UartEthRxStack
( uartEthRxStack
) where

import Clash.Cores.Ethernet.PacketStream ( PacketStream, PacketStreamM2S(..) )
import Clash.Cores.Ethernet.RGMII
import Clash.Prelude
import Clash.Cores.Ethernet.RxStack (rxStack)
import Protocols
import Data.Proxy
import Clash.Cores.UART
import Clash.Lattice.ECP5.UART
import Protocols.Internal (CSignal(CSignal))
import Data.Maybe (fromJust)

import Clash.Lattice.ECP5.Colorlight.CRG
import Clash.Lattice.ECP5.Prims

import Protocols.DfConv (fifo)

extractSignal :: CSignal dom a -> Signal dom a
extractSignal (CSignal x) = x

uartEthRxStack
  :: forall (dom :: Domain) (domEth :: Domain) (domDDREth :: Domain) (baud :: Nat)
   . (ValidBaud dom baud)
  => SNat baud
  -> RGMIIRXChannel domEth domDDREth
  -> Signal dom Bit
uartEthRxStack baud channel = extractSignal uartTxBit
  where
    rgmiiRx = rgmiiReceiver eth0_rx (delayg d80) iddrx1f channel
    packetStream = fmap rgmiiRecvToPacketStream rgmiiRx
    
    ckt :: Circuit (PacketStream dom 1 ()) (CSignal dom Bit)
    ckt = rxStack
      |> fifo Proxy Proxy (SNat @16000)
      |> uartTxNoBaudGenC (baudGenerator baud)

    (_, uartTxBit) = toSignals ckt (packetStream, pure ())


-- uartEthRxStack :: RGMIIRXChannel DomEth1 DomDDREth1 -> Signal dom Bit
-- uartEthRxStack channel = rgmiiReceiver channel _ _ |> DfConv.fifo Proxy Proxy (P.SNat @16000) |> uartTxNoBaudGenC

rgmiiRecvToPacketStream :: (Bool, Maybe (BitVector 8)) -> Maybe (PacketStreamM2S 1 ())
rgmiiRecvToPacketStream input = 
    let (err, dat) = input
        dat' = fromJust dat
    in case dat of
  Nothing -> Nothing
  _       -> Just $ PacketStreamM2S {_data = pure dat', _last = Nothing, _meta = (), _abort = err}


