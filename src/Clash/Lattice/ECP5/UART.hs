{-# language FlexibleContexts #-}
{-# language NamedFieldPuns #-}

module Clash.Lattice.ECP5.UART
  ( uartTxC
  , uartTxNoBaudGenC
  , unsafeUartRxC
  , unsafeUartRxNoBaudGenC
  , unsafeUartRxC'
  , unsafeUartRxNoBaudGenC'
  , toPacketsC
  ) where

import Clash.Cores.Ethernet.PacketStream
import Clash.Cores.UART
import Clash.Prelude
import Protocols
import Protocols.Internal
import Data.Maybe

convertToTx :: Signal dom (Maybe (PacketStreamM2S 1 ())) -> Signal dom (Maybe (BitVector 8))
convertToTx = fmap $ fmap (head . _data)

uartTxC
  :: forall (dom :: Domain)
            (baud :: Nat)
   . HiddenClockResetEnable dom
  => ValidBaud dom baud
  => SNat baud
  -- ^ The UART baud
  -> Circuit (PacketStream dom 1 ()) (CSignal dom Bit)
  -- ^ This component receives a PacketStream and converts it to the UART transmitter input while relaying backpressure from the UART
uartTxC baud = uartTxNoBaudGenC (baudGenerator baud)

uartTxNoBaudGenC
  :: HiddenClockResetEnable dom
  => BaudGenerator dom
  -- ^ The UART baud
  -> Circuit (PacketStream dom 1 ()) (CSignal dom Bit)
  -- ^ This component receives a PacketStream and converts it to the UART transmitter input while relaying backpressure from the UART
uartTxNoBaudGenC baudGen = fromSignals ckt
  where
    ckt (fwd, _) =  (PacketStreamS2M <$> ack, CSignal txBit)
      where
        (txBit, ack) = uartTxNoBaudGen baudGen (convertToTx fwd)

-- | State for `toPacketsC`
data ToPacketsState
  = ReadSize1 
  -- ^ Reading first size byte
  | ReadSize2 (BitVector 8) 
  -- ^ Reading second size byte
  | ReadData (BitVector 16)
  -- ^ Reading data
  deriving (Generic, NFDataX)

-- | Turns a stream of raw bytes into a stream of packets according to the following "protocol":
-- Packets consist of two bytes interpreted as a 16-bit integer n, followed by n + 1 bytes x_0, ..., x_n,
-- i.e. n is the index of the last byte of this packet.
-- _data, _meta and _abort of x_0, ..., x_n are copied to the output signal. _last is Just 0 only for x_n.
-- For the size bytes, _last, _meta and _abort are ignored.
-- n is read in little-endian byte order.
toPacketsC
  :: HiddenClockResetEnable dom
  => KnownDomain dom
  => Circuit (PacketStream dom 1 ()) (PacketStream dom 1 ())
toPacketsC = fromSignals ckt 
  where
    ckt (fwdInS, bwdInS) = (bwdInS, mealy go ReadSize1 fwdInS)
    go :: ToPacketsState -> Maybe (PacketStreamM2S 1 metaType) -> (ToPacketsState, Maybe (PacketStreamM2S 1 metaType))
    go s Nothing = (s, Nothing)
    go ReadSize1 (Just (PacketStreamM2S {_data})) = (ReadSize2 (head _data), Nothing)
    go (ReadSize2 size) (Just (PacketStreamM2S {_data})) = (ReadData (head _data ++# size), Nothing)
    go (ReadData size) (Just packetStream) = (s', Just packetStream {_last = _last'})
      where
        (s', _last') = if size == 0
                         then (ReadSize1, Just 0)
                         else (ReadData (size - 1), Nothing)

-- | UART receiver circuit
unsafeUartRxC
  :: HiddenClockResetEnable dom
  => ValidBaud dom baud
  => SNat baud
  -> Circuit (CSignal dom Bit) (PacketStream dom 1 ())
unsafeUartRxC = unsafeUartRxNoBaudGenC . baudGenerator

-- | UART receiver circuit
--
-- This version requires the baud generator to be passed in. Create one
-- using `Clash.Cores.UART.baudGenerator`.
unsafeUartRxNoBaudGenC
  :: forall (dom :: Domain)
   . HiddenClockResetEnable dom
  => BaudGenerator dom
  -> Circuit (CSignal dom Bit) (PacketStream dom 1 ())

unsafeUartRxNoBaudGenC baudGen = fromSignals ckt
  where
    ckt :: (Fwd (CSignal dom Bit), Bwd (PacketStream dom 1 ())) -> (Bwd (CSignal dom Bit), Fwd (PacketStream dom 1 ()))
    ckt (CSignal rxBit, _) = (def, convert $ uartRxNoBaudGen baudGen rxBit)

    convert :: Signal dom (Maybe (BitVector 8)) -> Fwd (PacketStream dom 1 ())
    convert = fmap $ fmap $ \x -> PacketStreamM2S (repeat x) Nothing () False
  
-- | UART receiver circuit interpreting packets. See also `toPacketsC`.
unsafeUartRxC'
  :: HiddenClockResetEnable dom
  => ValidBaud dom baud
  => SNat baud
  -> Circuit (CSignal dom Bit) (PacketStream dom 1 ())
unsafeUartRxC' baud =  unsafeUartRxC baud |> toPacketsC

-- | UART receiver circuit interpreting packets. See also `toPacketsC`.
--
-- This version requires the baud generator to be passed in. Create one
-- using `Clash.Cores.UART.baudGenerator`.
unsafeUartRxNoBaudGenC'
  :: HiddenClockResetEnable dom
  => BaudGenerator dom
  -> Circuit (CSignal dom Bit) (PacketStream dom 1 ())
unsafeUartRxNoBaudGenC' baudGen = unsafeUartRxNoBaudGenC baudGen |> toPacketsC
