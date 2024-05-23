{-|
  Copyright   :  (C) 2024, Tim Wallet <tw.personal@tutanota.com>
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

A fully modular MAC transmit stack which allows the transmission of data over Ethernet II. Supports any data width >= 1.

Example usage:

>>> import Clash.Prelude
>>> import Protocols
>>> import Clash.Cores.Ethernet.AsyncFIFO
>>> import Clash.Cores.Ethernet.EthernetTypes
>>> import Clash.Cores.Ethernet.DownConverter
>>> import Clash.Cores.Ethernet.FrameCheckSequence
>>> import Clash.Cores.Ethernet.InterpacketGapInserter
>>> import Clash.Cores.Ethernet.MacPacketizer
>>> import Clash.Cores.Ethernet.PacketStream
>>> import Clash.Cores.Ethernet.PaddingInserter
>>> import Clash.Cores.Ethernet.PreambleInserter

The Ethernet TX PHY is completely interchangable with this stack. In the example below, we use a dummy.
You can replace this dummy variable with any Ethernet TX PHY circuit that is adapted to the `PacketStream` protocol, i.e. with type:

>>> :{
dummyTxPhy :: Circuit (PacketStream domEthTx 1 ()) (PacketStream domEthTx 1 ())
dummyTxPhy = undefined
:}

Individual components are easily glued together using the '|>' operator, creating one big `Circuit`.
Due to this modularity, it is very easy to create a custom stack.
All components that before `asyncFifoC` handle any data width of choice. For example, let's say you
want a data width of 4. Achieving this is as specifiying the type of the `Circuit`:

>>> :{
macTxStack :: Circuit (PacketStream dom 4 EthernetHeader) (PacketStream domEthTx 1 ())
macTxStack =
  macPacketizerC
  |> paddingInserterC d60
  |> fcsInserterC
  |> preambleInserterC
  |> asyncFifoC d4 hasClock hasReset hasEnable ethTxClk ethTxRst ethTxEn
  |> (exposeClockResetEnable downConverterC ethTxClk ethTxRst ethTxEn)
  |> (exposeClockResetEnable interpacketGapInserterC ethTxClk ethTxRst ethTxEn) d12
  |> dummyTxPhy
:}

The payload stream together with an `EthernetHeader` in the metadata arrives at the `macPacketizerC`,
which prepends this header to the stream.

`paddingInserterC` pads the stream with null bytes to 60 bytes,
because the size of the ethernet header is 14 bytes and the Ethernet II payload needs to be at least 46
bytes. The amount of padding is configurable, if for whatever reason you need to pad frames to a different
size.

The resulting stream passes through `fcsInserterC`, which calculates
the Ethernet CRC-32 over the payload and Ethernet header together and appends it to the stream.

The last real manipulation on the stream is the insertion of the preamble to the front of the stream
by `preambleInserterC`, that is, 7 bytes of alternating ones and zeroes followed by the start frame delimiter.

Because the clock domain of the Ethernet TX PHY is usually different from the clock domain on the FPGA internally,
`asyncFifoC` is used to cross clock domains.

Then, `downConverterC` turns the stream from dataWidth n to dataWidth 1.

Lastly, the interpacket gap is inserted. The number of clock cycles is configurable, but you usually want the standard
interpacket gap of 12 clock cycles, which amounts to an interpacket gap of 96 bytes.

-}

module Clash.Cores.Ethernet.Examples.ExampleMacTxStack
  where
