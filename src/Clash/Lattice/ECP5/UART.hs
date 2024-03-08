{-#language FlexibleContexts #-}

module Clash.Lattice.ECP5.UART (uartTx') where

import Data.Maybe
import Clash.Cores.Ethernet.PacketStream
import Clash.Prelude
import Protocols
import Clash.Cores.UART

convert :: Fwd (PacketStream dom 1 ()) -> Signal dom (Maybe (BitVector 8))
convert = fmap $ fmap $ (head . _data)

uartTx'
  :: forall (dom :: Domain)
            (baud :: Nat)
   . HiddenClockResetEnable dom
  => ValidBaud dom baud
  => SNat baud
  -- ^ The UART baud
  -> Signal dom (Maybe (PacketStreamM2S 1 ()))
  -- ^ The word to transmit
  -> (Signal dom Bit, Signal dom Bool)
  -- ^ The output bits for the tx line and an acknowledgement that the word
  -- to transmit has been received
uartTx' baud mWord = uartTx baud (convert mWord)
