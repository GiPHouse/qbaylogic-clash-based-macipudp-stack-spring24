{-# language FlexibleContexts #-}

{-|
Module      : Clash.Cores.Ethernet.Examples.TxStack
Description : Provides the entire transmit stack as a circuit.
-}
module Clash.Cores.Ethernet.Examples.TxStack
  ( txStack
  ) where

import Clash.Cores.Crc
import Clash.Cores.Crc.Catalog

import Clash.Cores.Ethernet.Mac.EthernetTypes
import Clash.Cores.Ethernet.Mac.FrameCheckSequence ( fcsInserterC )
import Clash.Cores.Ethernet.Mac.InterpacketGapInserter ( interpacketGapInserterC )
import Clash.Cores.Ethernet.Mac.MacPacketizers ( macPacketizerC )
import Clash.Cores.Ethernet.Mac.PaddingInserter ( paddingInserterC )
import Clash.Cores.Ethernet.Mac.PreambleInserter ( preambleInserterC )
import Clash.Prelude
import Protocols ( Circuit, (|>) )
import Protocols.Extra.PacketStream ( PacketStream )
import Protocols.Extra.PacketStream.AsyncFIFO ( asyncFifoC )
import Protocols.Extra.PacketStream.DownConverter ( downConverterC )

-- | Processes bytes to send over ethernet
txStack
  :: forall (dataWidth :: Nat) (dom :: Domain) (domEth :: Domain)
   . ( KnownNat dataWidth
     , 1 <= dataWidth
     , HiddenClockResetEnable dom
     , KnownDomain domEth
     , HardwareCrc Crc32_ethernet 8 dataWidth
     )
  => Clock domEth
  -> Reset domEth
  -> Enable domEth
  -> Circuit (PacketStream dom dataWidth EthernetHeader) (PacketStream domEth 1 ())
txStack ethClk ethRst ethEn = ckt
  where
    ckt = macPacketizerC
      |> paddingInserterC d60
      |> fcsInserterC
      |> preambleInserterC
      |> asyncFifoC d4 hasClock hasReset hasEnable ethClk ethRst ethEn
      |> (exposeClockResetEnable downConverterC ethClk ethRst ethEn)
      |> (exposeClockResetEnable interpacketGapInserterC ethClk ethRst ethEn) d12
