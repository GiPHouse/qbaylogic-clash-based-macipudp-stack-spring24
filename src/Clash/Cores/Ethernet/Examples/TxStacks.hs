{-|
  Module      : Clash.Cores.Ethernet.Examples.TxStacks
  Copyright   : (C) 2024, Tim Wallet
  Description : Provides standard transmit stacks.
  License     : BSD2 (see the file LICENSE)
  Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

A fully modular MAC transmit stack which allows the transmission of data over Ethernet II.
Supports any data width bigger than zero.

Example usage:

>>> :set -XFlexibleContexts
>>> :set -XMultiParamTypeClasses
>>> import Clash.Prelude
>>> import Clash.Cores.Crc
>>> import Clash.Cores.Crc.Catalog
>>> import Clash.Cores.Ethernet.Mac.EthernetTypes
>>> import Data.Proxy
>>> import Protocols
>>> import Protocols.Extra.PacketStream

The Ethernet TX PHY is completely interchangeable with this stack. In the example below,
we use a dummy. You have to replace this dummy variable with an Ethernet TX PHY circuit
for your specific hardware (e.g. RGMII, MII or SGMII) that is adapted to the
`PacketStream` protocol, i.e. with type:

>>> :{
dummyTxPhy
  :: HiddenClockResetEnable domEthTx
  => Circuit (PacketStream domEthTx 1 ()) (PacketStream domEthTx 1 ())
dummyTxPhy = undefined
:}

For example, the Lattice ECP5 board uses an RGMII PHY, found at `Clash.Lattice.ECP5.RGMII.rgmiiTxC`.

`macTxStack` is the most common ethernet MAC TX stack that will be sufficient for
most people. That is, it inserts an interpacket gap of 12 bytes, pads the payload
to 46 bytes and assumes that you process the bytes in a different clock domain than the
ethernet TX domain. All you have to do is specify the data width (in this example 4),
the clock domains, and the TX PHY you want to use.

The stack uses `Clash.Cores.Crc.crcEngine` internally to calculate the frame check
sequence of the Ethernet frame. To be able to use this component, we need to use
`Clash.Cores.Crc.deriveHardwareCrc` to derive the necessary instance.

>>> :{
$(deriveHardwareCrc (Proxy @Crc32_ethernet) d8 d4)
myTxStack
  :: HiddenClockResetEnable dom
  => KnownDomain domEthTx
  => Clock domEthTx
  -> Reset domEthTx
  -> Enable domEthTx
  -> Circuit (PacketStream dom 4 EthernetHeader) (PacketStream domEthTx 1 ())
myTxStack ethTxClk ethTxRst ethTxEn =
  macTxStack @4 ethTxClk ethTxRst ethTxEn
  |> exposeClockResetEnable dummyTxPhy ethTxClk ethTxRst ethTxEn
:}

Need a TX stack that does it a little different? In this case, you can easily create a
custom stack by importing the individual components and connecting them via the `|>`
operator, creating one big `Circuit`. For example:

>>> import Protocols.Extra.PacketStream.Packetizers
>>> import Clash.Cores.Ethernet.Mac.FrameCheckSequence
>>> import Clash.Cores.Ethernet.Mac.InterpacketGapInserter
>>> import Clash.Cores.Ethernet.Mac.PaddingInserter
>>> import Clash.Cores.Ethernet.Mac.Preamble

This custom TX stack processes bytes in the ethernet TX domain. In this case, we can omit
`asyncFifoC` and `downConverterC`. We also use a bigger interpacket gap than usual, i.e. 16 bytes.

>>> :{
myCustomTxStack
  :: HiddenClockResetEnable domEthTx
  => HardwareCrc Crc32_ethernet 8 1
  => Circuit (PacketStream domEthTx 1 EthernetHeader) (PacketStream domEthTx 1 ())
myCustomTxStack =
  macPacketizerC
  |> paddingInserterC d60
  |> fcsInserterC
  |> preambleInserterC
  |> interpacketGapInserterC d16
  |> dummyTxPhy
:}

-}

{-# language FlexibleContexts #-}

module Clash.Cores.Ethernet.Examples.TxStacks
  ( macTxStack
  , ipTxStack
  ) where

import Clash.Prelude

import Protocols ( Circuit, (|>) )
import Protocols.Extra.PacketStream ( PacketStream )
import Protocols.Extra.PacketStream.AsyncFIFO ( asyncFifoC )
import Protocols.Extra.PacketStream.Converters ( downConverterC )

import Clash.Cores.Crc ( HardwareCrc )
import Clash.Cores.Crc.Catalog ( Crc32_ethernet )
import Clash.Cores.Ethernet.IP.IPPacketizers
import Clash.Cores.Ethernet.IP.IPv4Types
import Clash.Cores.Ethernet.Mac.EthernetTypes
import Clash.Cores.Ethernet.Mac.FrameCheckSequence
import Clash.Cores.Ethernet.Mac.InterpacketGapInserter ( interpacketGapInserterC )
import Clash.Cores.Ethernet.Mac.MacPacketizers ( macPacketizerC )
import Clash.Cores.Ethernet.Mac.PaddingInserter ( paddingInserterC )
import Clash.Cores.Ethernet.Mac.Preamble


-- | Processes bytes to transmit over Ethernet. This is done in the following way:
--
-- 1. The payload stream together with an `EthernetHeader` in the metadata arrives at
-- `macPacketizerC`, which prepends this header to the stream. This header contains the
-- source and destination MAC addresses, and the EtherType of the payload.
--
-- 2. `paddingInserterC` pads the stream to 60 bytes with null bytes, because the size
-- bytes long.
--
-- 3. The resulting stream passes through `fcsInserterC`, which calculates the Ethernet
-- CRC-32 over the payload and Ethernet header together and appends it to the stream.
--
-- 4. The last real manipulation on the stream is the insertion of the preamble to the
-- front of the stream by `preambleInserterC`, that is, 7 bytes of alternating ones and
-- zeroes followed by the start frame delimiter.
--
-- 5. Because the clock domain of the Ethernet TX PHY is usually different from the clock
-- domain on the FPGA internally, `asyncFifoC` is used to cross clock domains.
--
-- 6. Then, `downConverterC` turns the stream from dataWidth n to dataWidth 1.
--
-- 7. Lastly, an interpacket gap of 12 bytes is inserted.
macTxStack
  :: forall
     (dataWidth :: Nat)
     (dom :: Domain)
     (domEthTx :: Domain)
   . HiddenClockResetEnable dom
  => KnownDomain domEthTx
  => HardwareCrc Crc32_ethernet 8 dataWidth
  => KnownNat dataWidth
  => 1 <= dataWidth
  => Clock domEthTx
  -- ^ Clock signal in the Ethernet TX domain
  -> Reset domEthTx
  -- ^ Reset signal in the Ethernet TX domain
  -> Enable domEthTx
  -- ^ Enable signal in the Ethernet TX domain
  -> Circuit (PacketStream dom dataWidth EthernetHeader) (PacketStream domEthTx 1 ())
macTxStack ethTxClk ethTxRst ethTxEn =
  macPacketizerC
  |> paddingInserterC d60
  |> fcsInserterC
  |> preambleInserterC
  |> asyncFifoC d4 hasClock hasReset hasEnable ethTxClk ethTxRst ethTxEn
  |> exposeClockResetEnable downConverterC ethTxClk ethTxRst ethTxEn
  |> exposeClockResetEnable interpacketGapInserterC ethTxClk ethTxRst ethTxEn d12

-- | Sends IP packets to a known mac address
ipTxStack
  :: forall
     (dataWidth :: Nat)
     (dom :: Domain)
     (domEthTx :: Domain)
   . HiddenClockResetEnable dom
  => KnownDomain domEthTx
  => HardwareCrc Crc32_ethernet 8 dataWidth
  => KnownNat dataWidth
  => 1 <= dataWidth
  => Clock domEthTx
  -- ^ Clock signal in the Ethernet TX domain
  -> Reset domEthTx
  -- ^ Reset signal in the Ethernet TX domain
  -> Enable domEthTx
  -- ^ Enable signal in the Ethernet TX domain
  -> Signal dom MacAddress
  -- ^ Our MAC address
  -> Circuit (PacketStream dom dataWidth IPv4HeaderLite) (PacketStream domEthTx 1 ())
ipTxStack ethTxClk ethTxRst ethTxEn ourMacS =
  ipLitePacketizerC
  |> toEthernetC ourMacS
  |> macTxStack ethTxClk ethTxRst ethTxEn
