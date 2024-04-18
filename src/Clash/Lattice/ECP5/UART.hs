{-# language FlexibleContexts #-}
{-# language NamedFieldPuns #-}

{-|
Module      : Clash.Lattice.ECP5.UART
Description : Wrappers of the Clash UART core into the packet stream protocol
-}
module Clash.Lattice.ECP5.UART
  ( uartTxC
  , uartTxNoBaudGenC
  , uartRxC
  , uartRxNoBaudGenC
  , uartRxC'
  , uartRxNoBaudGenC'
  , toPacketsC
  ) where

import Clash.Cores.Ethernet.PacketStream
import Clash.Cores.UART
import Clash.Prelude
import Protocols
import Protocols.Internal

convertToTx :: Signal dom (Maybe (PacketStreamM2S 1 ())) -> Signal dom (Maybe (BitVector 8))
convertToTx = fmap $ fmap (head . _data)

-- | UART transmitter circuit
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

-- | UART transmitter circuit
--
-- This version requires the baud generator to be passed in. Create one
-- using `Clash.Cores.UART.baudGenerator`.
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
  :: forall (dom :: Domain) (metaType :: Type)
   . HiddenClockResetEnable dom
  => KnownDomain dom
  => Circuit (CSignal dom (Maybe (PacketStreamM2S 1 metaType))) (CSignal dom (Maybe (PacketStreamM2S 1 metaType)))
toPacketsC = fromSignals ckt
  where
    ckt
      :: (CSignal dom (Maybe (PacketStreamM2S 1 metaType)), CSignal dom ())
      -> (CSignal dom (), CSignal dom (Maybe (PacketStreamM2S 1 metaType)))
    ckt (CSignal fwdInS, _) = (CSignal $ pure (), CSignal (mealy go ReadSize1 fwdInS))
    go :: ToPacketsState -> Maybe (PacketStreamM2S 1 metaType) -> (ToPacketsState, Maybe (PacketStreamM2S 1 metaType))
    go s Nothing = (s, Nothing)
    go ReadSize1 (Just (PacketStreamM2S {_data})) = (ReadSize2 (head _data), Nothing)
    go (ReadSize2 size) (Just (PacketStreamM2S {_data})) = (ReadData (head _data ++# size), Nothing)
    go (ReadData 0) (Just packetStream) = (ReadSize1, Just packetStream {_last = Just 0})
    go (ReadData size) (Just packetStream) = (ReadData (size - 1), Just packetStream {_last = Nothing})

-- | UART receiver circuit
uartRxC
  :: HiddenClockResetEnable dom
  => ValidBaud dom baud
  => SNat baud
  -> Circuit (CSignal dom Bit) (CSignal dom (Maybe (PacketStreamM2S 1 ())))
uartRxC = uartRxNoBaudGenC . baudGenerator

-- | UART receiver circuit
--
-- This version requires the baud generator to be passed in. Create one
-- using `Clash.Cores.UART.baudGenerator`.
uartRxNoBaudGenC
  :: forall (dom :: Domain)
   . HiddenClockResetEnable dom
  => BaudGenerator dom
  -> Circuit (CSignal dom Bit) (CSignal dom (Maybe (PacketStreamM2S 1 ())))
uartRxNoBaudGenC baudGen = fromSignals ckt
  where
    ckt :: (CSignal dom Bit, CSignal dom ()) -> (CSignal dom (), CSignal dom (Maybe (PacketStreamM2S 1 ())))
    ckt (CSignal rxBit, _) = (def, CSignal $ convert $ uartRxNoBaudGen baudGen rxBit)

    convert :: Signal dom (Maybe (BitVector 8)) -> Signal dom (Maybe (PacketStreamM2S 1 ()))
    convert = fmap $ fmap $ \x -> PacketStreamM2S (repeat x) Nothing () False

-- | UART receiver circuit interpreting packets with `toPacketsC`.
uartRxC'
  :: HiddenClockResetEnable dom
  => ValidBaud dom baud
  => SNat baud
  -> Circuit (CSignal dom Bit) (CSignal dom (Maybe (PacketStreamM2S 1 ())))
uartRxC' baud =  uartRxC baud |> toPacketsC

-- | UART receiver circuit interpreting packets with `toPacketsC`.
--
-- This version requires the baud generator to be passed in. Create one
-- using `Clash.Cores.UART.baudGenerator`.
uartRxNoBaudGenC'
  :: HiddenClockResetEnable dom
  => BaudGenerator dom
  -> Circuit (CSignal dom Bit) (CSignal dom (Maybe (PacketStreamM2S 1 ())))
uartRxNoBaudGenC' baudGen = uartRxNoBaudGenC baudGen |> toPacketsC
