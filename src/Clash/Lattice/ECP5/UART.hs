{-# language FlexibleContexts #-}
{-# language RecordWildCards #-}

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

data MarkLastState = Idle | ReadSize (BitVector 8) | ReadData (BitVector 16)
  deriving (Generic, NFDataX)

-- | Turns a stream of raw bytes into a stream of packets according to the following "protocol".
-- Packets consist of two bytes interpreted as a 16-bit integer n, followed by n + 1 bytes x_0, ..., x_n.
-- x_0, ..., x_n are sent to the output signal and for the byte x_n, _last is set.
-- n is read in little-endian byte order.
-- >>> Clash.Signal.simulate (markLastFlag @Clash.Prelude.System) ([Just (PacketStreamM2S @1 @() (2:>Nil) Nothing () False), Just (PacketStreamM2S @1 @() (1:>Nil) Nothing () False)] Data.List.++ (Data.List.repeat (Just (PacketStreamM2S @1 @() (0xAA:>Nil) Nothing () False)))) !! 260
-- Just (PacketStreamM2S {_data = 0b1010_1010 :> Nil, _last = Just 0, _meta = (), _abort = False})
markLastFlag
  :: HiddenClockResetEnable dom
  => KnownDomain dom
  => Fwd (PacketStream dom 1 ())
  -- ^ Packet stream of raw bytes
  -> Fwd (PacketStream dom 1 ())
  -- ^ Packet stream with size bytes removed and _last flag added
markLastFlag = mealy go s0 where
    s0 :: MarkLastState
    s0 = Idle
    go :: MarkLastState -> Maybe (PacketStreamM2S 1 ()) -> (MarkLastState, Maybe (PacketStreamM2S 1 ()))
    go s Nothing = (s, Nothing)
    go Idle (Just (PacketStreamM2S {..})) = (ReadSize (head _data), Nothing)
    go (ReadSize size) (Just (PacketStreamM2S {..})) = (ReadData (head _data ++# size), Nothing)
    go (ReadData size) (Just (PacketStreamM2S {..})) =
      if size == 0
        then ( Idle
             , Just PacketStreamM2S
                 { _data = _data
                 , _last = Just 0
                 , _meta = ()
                 , _abort = False
                 }
             )
        else ( ReadData (size - 1)
             , Just PacketStreamM2S
                 { _data = _data
                 , _last = Nothing
                 , _meta = ()
                 , _abort = False
                 }
             )

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
