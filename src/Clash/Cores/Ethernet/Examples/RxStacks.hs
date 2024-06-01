{-# language FlexibleContexts #-}

{-|
Module      : Clash.Cores.Ethernet.Examples.RxStacks
Description : Provides the entire receive stack as a circuit.
-}
module Clash.Cores.Ethernet.Examples.RxStacks
  ( macRxStack
  , ipRxStack
  ) where

import Clash.Cores.Crc
import Clash.Cores.Crc.Catalog
import Clash.Prelude

import Protocols
import Protocols.Extra.PacketStream
import Protocols.Extra.PacketStream.AsyncFIFO ( asyncFifoC )
import Protocols.Extra.PacketStream.Converters ( upConverterC )
import Protocols.Extra.PacketStream.Routing ( packetDispatcherC )

import Clash.Cores.Ethernet.IP.IPPacketizers
import Clash.Cores.Ethernet.IP.IPv4Types
import Clash.Cores.Ethernet.Mac.EthernetTypes
import Clash.Cores.Ethernet.Mac.FrameCheckSequence ( fcsValidatorC )
import Clash.Cores.Ethernet.Mac.MacPacketizers ( macDepacketizerC )
import Clash.Cores.Ethernet.Mac.Preamble ( preambleStripperC )

-- | Processes received ethernet frames
macRxStack
  :: forall (dataWidth :: Nat) (dom :: Domain) (domEth :: Domain)
   . ( HiddenClockResetEnable dom
     , KnownDomain domEth
     , HardwareCrc Crc32_ethernet 8 dataWidth
     , KnownNat dataWidth
     , 1 <= dataWidth
     )
  => Clock domEth
  -> Reset domEth
  -> Enable domEth
  -> Signal dom MacAddress
  -> Circuit (PacketStream domEth 1 ()) (PacketStream dom dataWidth EthernetHeader)
macRxStack ethClk ethRst ethEn macAddressS =
    upConverterC'
    |> asyncFifoC'
    |> preambleStripperC
    |> fcsValidatorC
    |> macDepacketizerC
    |> filterMetaS (isForMyMac <$> macAddressS)
  where
    upConverterC' = exposeClockResetEnable upConverterC ethClk ethRst ethEn
    asyncFifoC' = asyncFifoC d4 ethClk ethRst ethEn hasClock hasReset hasEnable
    isForMyMac myMac (_macDst -> to) = to == myMac || to == broadcastMac

-- | Processes received IP packets
ipRxStack
  :: forall (dataWidth :: Nat) (dom :: Domain) (domEth :: Domain)
   . ( HiddenClockResetEnable dom
     , KnownDomain domEth
     , HardwareCrc Crc32_ethernet 8 dataWidth
     , KnownNat dataWidth
     , 1 <= dataWidth
     )
  => Clock domEth
  -> Reset domEth
  -> Enable domEth
  -> Signal dom MacAddress
  -> Signal dom (IPv4Address, IPv4Address)
  -> Circuit (PacketStream domEth 1 ()) (PacketStream dom dataWidth IPv4HeaderLite)
ipRxStack ethClk ethRst ethEn macAddressS ipS = circuit $ \raw -> do
  ethernetFrames <- macRxStack ethClk ethRst ethEn macAddressS -< raw
  [ip] <- packetDispatcherC (isIpv4 :> Nil) -< ethernetFrames
  ipDepacketizerLiteC |> filterMetaS (isForMyIp <$> ipS) -< ip
  where
    isIpv4 = (== 0x0800) . _etherType
    isForMyIp (ip, subnet) (_ipv4lDestination -> to) = to == ip || to == ipv4Broadcast ip subnet
