{-|
  Module      : Clash.Cores.Ethernet.Examples.RxStack
  Copyright   :
  Description : Provides a standard Ethernet MAC receive stack.
  License     : BSD2 (see the file LICENSE)
  Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

A fully modular MAC receive stack which allows for the reception of data over Ethernet II.
Supports any data width greater than zero.

Example usage:

Start by importing some necessary components:

>>> :set -XFlexibleContexts
>>> :set -XMultiParamTypeClasses
>>> import Data.Proxy
>>> import Clash.Cores.Crc
>>> import Clash.Cores.Crc.Catalog
>>> import Clash.Prelude
>>> import Protocols
>>> import Protocols.Extra.PacketStream
>>> import Protocols.Extra.PacketStream.AsyncFIFO
>>> import Protocols.Extra.PacketStream.Converters
>>> import Clash.Cores.Ethernet.Mac.EthernetTypes
>>> import Clash.Cores.Ethernet.Mac.FrameCheckSequence
>>> import Clash.Cores.Ethernet.Mac.MacPacketizers
>>> import Clash.Cores.Ethernet.Mac.Preamble

To use this stack, you need an ethernet RX PHY. For an example, see
`Clash.Lattice.ECP5.RGMII.unsafeRgmiiRxC`, which is an RGMII for the
Lattice ECP5 board. To use your own PHY, it needs to be adapted to
the `PacketStream` protocol, so it needs to have the type:

>>> :{
dummyRxPhy
  :: HiddenClockResetEnable domEthRx
  => Circuit (PacketStream domEthRx 1 ()) (PacketStream domEthRx 1 ())
dummyRxPhy = undefined
:}

We will use this dummy PHY for our examples, which you should replace
with your own PHY circuit.

This module provides the most common ethernet MAC RX stack, which is
sufficient for most cases. It first merges the packetstreams so that
the stack can be used with slower clock cycles. Then, it sends these packets
to an async FIFO to cross to a different clock domain. The MAC packets
are then processed, which includes stripping the preamble, validating
the frame check sequence, and finally parses the first 14 bytes into
an ethernet header. The stack also filters frames for the given mac
address.

The stack uses `Clash.Cores.Crc.crcEngine` internally to calculate the frame check
sequence of the ethernet frame. To be able to use this component, we need to use
`Clash.Cores.Crc.deriveHardwareCrc` to derive the necessary instance.

The complete stack, with a data width of 4, can be used with:
>>> :{
$(deriveHardwareCrc (Proxy @Crc32_ethernet) d8 d4)
myRxStack
  :: HiddenClockResetEnable dom
  => KnownDomain domEthRx
  => Clock domEthRx
  -> Reset domEthRx
  -> Enable domEthRx
  -> Signal dom MacAddress
  -> Circuit (PacketStream domEthRx 1 ()) (PacketStream dom 4 EthernetHeader)
myRxStack ethRxClk ethRxRst ethRxEn macAddressS =
  exposeClockResetEnable dummyRxPhy ethRxClk ethRxRst ethRxEn
  |> macRxStack @4 ethRxClk ethRxRst ethRxEn macAddressS
:}

You can also create a custom RX stack, by combining circuits using the
`|>` operator. For example, if you want just a basic stack to process
frames in the ethernet RX domain, you don't need `upConverterC` or `asyncFifoC`:

myCustomRxStack
  :: HiddenClockResetEnable domEthRx
  => HardwareCrc Crc32_ethernet 8 dataWidth
  -> Circuit (PacketStream domEthRx 1 ()) (PacketStream domEthRx 1 EthernetHeader)
myCustomRxStack =
  exposeClockResetEnable dummyRxPhy ethRxClk ethRxRst ethRxEn
  |> preambleStripperC
  |> fcsValidatorC
  |> macDepacketizerC

If you want to only use MAC, then you are now ready. If you also want to use IP,
then you should continue reading.

TODO: Add IP stack example

-}

{-# language FlexibleContexts #-}

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

-- | Processes received ethernet frames in the following way:
--
-- 1. `upConverterC` merges the packets to packets of the given data width
--
-- 2. Because the clock domain of the Ethernet RX PHY is usually different from the
-- clock domain on the FPGA internally, `asyncFifoC` is used to cross clock domains.
--
-- 3. The first real manipulation of the stream is stripping the preamble from the
-- front of the frame by `preambleStripperC`.
--
-- 4. `fcsValidatorC calculates and validates the frame check sequence in the
-- ethernet header, and sets an abort if it is incorrect.
--
-- 5. `macDepacketizerC` parses the header from the front of the frame and puts it
-- in the metadata of the stream.
--
-- 6. The MAC address of the frame is compared to the given address, and the packet
-- is dropped if we are not the recepient.
macRxStack
  :: forall (dataWidth :: Nat) (dom :: Domain) (domEthRx :: Domain)
   . ( HiddenClockResetEnable dom
     , KnownDomain domEthRx
     , HardwareCrc Crc32_ethernet 8 dataWidth
     , KnownNat dataWidth
     , 1 <= dataWidth
     )
  => Clock domEthRx
  -> Reset domEthRx
  -> Enable domEthRx
  -> Signal dom MacAddress
  -> Circuit (PacketStream domEthRx 1 ()) (PacketStream dom dataWidth EthernetHeader)
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
