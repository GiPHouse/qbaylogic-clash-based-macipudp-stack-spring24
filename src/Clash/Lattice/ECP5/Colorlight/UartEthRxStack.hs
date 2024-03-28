{-# language FlexibleContexts #-}

module Clash.Lattice.ECP5.Colorlight.UartEthRxStack
( uartEthRxStack
) where

import Clash.Cores.Ethernet.PacketStream ( PacketStream, PacketStreamM2S(..) )
import Clash.Cores.Ethernet.RGMII
import Clash.Cores.Ethernet.RxStack ( rxStack )
import Clash.Cores.UART
import Clash.Lattice.ECP5.Colorlight.CRG
import Clash.Lattice.ECP5.Prims
import Clash.Lattice.ECP5.UART
import Clash.Prelude
import Clash.Signal ( HiddenClock, HiddenEnable, HiddenReset )
import Data.Maybe ( fromJust )
import Data.Proxy
import Protocols
import Protocols.Internal ( CSignal(CSignal) )

import Protocols.DfConv ( fifo )

extractSignal :: CSignal dom a -> Signal dom a
extractSignal (CSignal x) = x

-- uartEthRxStack
--   :: forall (domEth :: Domain)
--             (domDDREth :: Domain)
--             (dom :: Domain)
--             (baud :: Nat)
--             fPeriod
--             edge
--             reset
--             init
--             polarity
--    . (KnownConfiguration domDDREth ('DomainConfiguration domDDREth fPeriod edge reset init polarity)
--    , HiddenClockResetEnable domEth
--    , HiddenClockResetEnable domDDREth
--    , (ValidBaud domEth baud))
--   => KnownConfiguration domEth ('DomainConfiguration domEth (2*fPeriod) edge reset init polarity)
uartEthRxStack
  :: forall baud dom domEth0 domDDREth0
  . 
  ( KnownDomain domDDREth0
  , ValidBaud dom baud
  , KnownConf domEth0 ~ 'DomainConfiguration domEth0 8000 'Rising 'Asynchronous 'Unknown 'ActiveHigh
  , KnownConf domDDREth0 ~ 'DomainConfiguration
                        domDDREth0 4000 'Rising 'Asynchronous 'Unknown 'ActiveHigh, (
                       HiddenClockResetEnable domEth0,HiddenClockResetEnable dom, dom ~ domEth0 ))
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


