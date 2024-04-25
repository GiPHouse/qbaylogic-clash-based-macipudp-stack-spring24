{-# language FlexibleContexts #-}

{-|
Module      : Clash.Cores.Ethernet.TxStack
Description : Provides the entire transmit stack as a circuit
-}
module Clash.Cores.Ethernet.TxStack
  ( txStack
  ) where

import Clash.Cores.Crc
import Clash.Cores.Crc.Catalog

import Clash.Cores.Ethernet.AsyncFIFO ( asyncFifoC )
import Clash.Cores.Ethernet.DownConverter ( downConverterC )
import Clash.Cores.Ethernet.EthernetTypes
import Clash.Cores.Ethernet.FrameCheckSequence ( fcsInserterC )
import Clash.Cores.Ethernet.InterpacketGapInserter ( interpacketGapInserterC )
import Clash.Cores.Ethernet.MacPacketizer
import Clash.Cores.Ethernet.PacketStream ( PacketStream )
import Clash.Cores.Ethernet.PaddingInserter ( paddingInserterC )
import Clash.Cores.Ethernet.PreambleInserter ( preambleInserterC )
import Clash.Prelude
import Protocols ( Circuit, (|>) )

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
