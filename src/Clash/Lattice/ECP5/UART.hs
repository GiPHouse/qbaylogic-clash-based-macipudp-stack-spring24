{-# language FlexibleContexts #-}
{-# language NamedFieldPuns #-}

module Clash.Lattice.ECP5.UART
  ( markLastFlag
  , unsafeUartPacketSource
  , uartRx'
  , uartTxC
  , uartTxNoBaudGenC
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

convert :: Signal dom (Maybe (BitVector 8)) -> Fwd (PacketStream dom 1 ())
convert = fmap $ fmap $ \x -> PacketStreamM2S (repeat x) Nothing () False

uartRx'
  :: HiddenClockResetEnable dom
  => ValidBaud dom baud
  => SNat baud
  -- ^ The UART baud
  -> Signal dom Bit
  -- ^ The UART receive line
  -> Fwd (PacketStream dom 1 ())
  -- ^ The received words
uartRx' b s = convert $ uartRx b s

data MarkLastState = ReadSize1 | ReadSize2 (BitVector 8) | ReadData (BitVector 16)
  deriving (Generic, NFDataX)

-- | Turns a stream of raw bytes into a stream of packets according to the following "protocol":
-- Packets consist of two bytes interpreted as a 16-bit integer n, followed by n + 1 bytes x_0, ..., x_n,
-- i.e. n is the index of the last byte of this packet.
-- _data, _meta and _abort of x_0, ..., x_n are copied to the output signal. _last is Just 0 only for x_n.
-- For the size bytes, _last, _meta and _abort are ignored.
-- n is read in little-endian byte order.
--
-- >>> Clash.Signal.simulate (markLastFlag @Clash.Prelude.System) ([Just (PacketStreamM2S @1 @() (2:>Nil) Nothing () False), Just (PacketStreamM2S @1 @() (1:>Nil) Nothing () False)] Data.List.++ (Data.List.repeat (Just (PacketStreamM2S @1 @() (0xAA:>Nil) Nothing () False)))) !! 260
-- Just (PacketStreamM2S {_data = 0b1010_1010 :> Nil, _last = Just 0, _meta = (), _abort = False})
markLastFlag
  :: HiddenClockResetEnable dom
  => KnownDomain dom
  => Fwd (PacketStream dom 1 metaType)
  -- ^ Packet stream of raw bytes
  -> Fwd (PacketStream dom 1 metaType)
  -- ^ Output stream
markLastFlag = mealy go ReadSize1 where
    go :: MarkLastState -> Maybe (PacketStreamM2S 1 metaType) -> (MarkLastState, Maybe (PacketStreamM2S 1 metaType))
    go s Nothing = (s, Nothing)
    go ReadSize1 (Just (PacketStreamM2S {_data})) = (ReadSize2 (head _data), Nothing)
    go (ReadSize2 size) (Just (PacketStreamM2S {_data})) = (ReadData (head _data ++# size), Nothing)
    go (ReadData size) (Just packetStream) = (s', Just packetStream {_last = _last'})
      where
        (s', _last') = if size == 0
                         then (ReadSize1, Just 0)
                         else (ReadData (size - 1), Nothing)

unsafeUartPacketSource
  :: HiddenClockResetEnable dom
  => ValidBaud dom baud
  => SNat baud
  -- ^ The UART baud
  -> Signal dom Bit
  -- ^ The UART receive line
  -> Fwd (PacketStream dom 1 ())
  -- ^ The received words with marked last packet
unsafeUartPacketSource b s = markLastFlag $ uartRx' b s
