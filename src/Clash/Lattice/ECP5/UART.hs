{-# language FlexibleContexts #-}

module Clash.Lattice.ECP5.UART
    (uartTxC) where

import Data.Maybe

import Clash.Cores.Ethernet.PacketStream
import Clash.Cores.UART
import Clash.Prelude
import Protocols
import Protocols.Internal


convertToTx :: Signal dom (Maybe (PacketStreamM2S 1 ())) -> Signal dom (Maybe (BitVector 8))
convertToTx = fmap $ fmap $ (head . _data)

uartTxC
  :: forall (dom :: Domain)
            (baud :: Nat)
   . HiddenClockResetEnable dom
  => ValidBaud dom baud
  => SNat baud
  -- ^ The UART baud
  -> Circuit (PacketStream dom 1 ()) (CSignal dom Bit)
  -- ^ This component receives a PacketStream and converts it to the UART transmitter input while relaying backpressure from the UART
uartTxC baud = fromSignals ckt
  where
    ckt (fwd, _) =  (PacketStreamS2M <$> ack, CSignal txBit)
      where
        (txBit, ack) = uartTx baud (convertToTx fwd)
