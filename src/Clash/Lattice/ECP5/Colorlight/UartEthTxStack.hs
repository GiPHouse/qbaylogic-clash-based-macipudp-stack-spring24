{-# language FlexibleContexts #-}

module Clash.Lattice.ECP5.Colorlight.UartEthTxStack
  ( uartEthTxStack
  ) where

import Clash.Cores.Ethernet.PacketBuffer ( packetBufferC )
import Clash.Cores.Ethernet.PacketStream
import Clash.Cores.Ethernet.RGMII
import Clash.Cores.Ethernet.TxStack
import Clash.Cores.Ethernet.UpConverter
import Clash.Cores.Ethernet.DownConverter
import Clash.Cores.UART ( BaudGenerator )
import Clash.Lattice.ECP5.Prims ( delayg, oddrx1f )
import Clash.Lattice.ECP5.UART
import Clash.Prelude
import Protocols
import Protocols.Internal ( CSignal(CSignal) )
import Clash.Cores.Ethernet.AsyncFIFO (asyncFifoC)
import Clash.Cores.Ethernet.Util
import Data.Maybe (isJust, isNothing)
import Clash.Sized.Vector (unsafeFromList)
import Data.Char (ord)

uartEthTxStack
  :: forall (dom :: Domain) (domEth :: Domain) (domDDREth :: Domain)
   . ( KnownDomain dom
     , KnownDomain domEth
     , KnownDomain domDDREth
     , HiddenClockResetEnable dom
     , KnownConf domEth ~ 'DomainConfiguration domEth 8000 'Rising 'Asynchronous 'Unknown 'ActiveHigh
     , KnownConf domDDREth ~ 'DomainConfiguration domDDREth 4000 'Rising 'Asynchronous 'Unknown 'ActiveHigh)
  => Clock domEth
  -> Reset domEth
  -> BaudGenerator dom
  -> Signal dom Bit
  -> Signal domEth (Maybe (BitVector 8))
  -> RGMIITXChannel domDDREth
uartEthTxStack clkEth rstEth baudGen uartRxS bytes = channel
  where
    channel = rgmiiSender clkEth rstEth (delayg d0) oddrx1f maybeByteS
    ckt = uartRxNoBaudGenC' baudGen
          |> unsafeToPacketStream
          |> constFrame
          |> downConverterC
          |> upConverterC
          |> packetBufferC d10
          |> txStack @4 clkEth rstEth enableGen
    ckt' = exposeClockResetEnable ckt hasClock hasReset hasEnable
    (_, maybePacketStreamS) = toSignals ckt' (CSignal uartRxS, pure (PacketStreamS2M True))
    --(_, maybePacketStreamS) = toSignals ckt' (CSignal $ exposeClockResetEnable insertLast clkEth rstEth enableGen $ fmap (\x -> PacketStreamM2S (singleton x) Nothing () False) <$> bytes, pure (PacketStreamS2M True))
    maybeByteS = fmap (fmap (head . _data)) maybePacketStreamS

insertLast :: HiddenClockResetEnable dom => Signal dom (Maybe (PacketStreamM2S 1 ())) -> Signal dom (Maybe (PacketStreamM2S 1 ()))
insertLast dataIn = liftA2 (\x y -> fmap (\x' -> x' {_last = y}) x) dataOut lastOut where
    dataOut = register Nothing dataIn
    lastOut = toMaybe <$> (isJust <$> dataOut .&&. isNothing <$> dataIn) <*> (pure 0)

constFrame :: Circuit (PacketStream dom 1 ()) (PacketStream dom 71 ())
constFrame = Circuit go where
  go (_, _) = (pure $ PacketStreamS2M True, pure $ Just $ PacketStreamM2S (myData) (Just 70) () False)

myData :: Vec 71 (BitVector 8)
myData = $(listToVecTH (fromIntegral . ord <$> "UUUUUUU\xd5\x00\xe0Ok\xfb\xa3\x00\xe0Ok\xfb\xa3\xff\xffHello world! :D (please work)\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00nj\xd5" :: [BitVector 8]))
