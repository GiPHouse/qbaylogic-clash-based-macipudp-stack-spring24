{-# language FlexibleContexts #-}
{-# language MultiParamTypeClasses #-}

module Clash.Cores.Ethernet.TxStack
  ( txStack
  ) where

import Clash.Cores.Crc
import Clash.Cores.Crc.Catalog

import Clash.Cores.Ethernet.AsyncFIFO ( asyncFifoC )
import Clash.Cores.Ethernet.DownConverter ( downConverterC )
import Clash.Cores.Ethernet.EthernetTypes
import Clash.Cores.Ethernet.FcsInserter ( fcsInserterC )
import Clash.Cores.Ethernet.InterpacketGapInserter ( interpacketGapInserterC )
import Clash.Cores.Ethernet.Packetizer ( packetizerC )
import Clash.Cores.Ethernet.PacketStream ( PacketStream )
import Clash.Cores.Ethernet.PaddingInserter ( paddingInserterC )
import Clash.Cores.Ethernet.PreambleInserter ( preambleInserterC )
import Clash.Prelude
import Protocols

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
  -> Circuit (PacketStream dom dataWidth ()) (PacketStream domEth 1 ())
txStack ethClk ethRst ethEn
  =  packetizerC (const ()) (const header)
  |> paddingInserterC d60
  |> fcsInserterC
  |> preambleInserterC
  |> asyncFifoC'
  |> downConverterC'
  |> interpacketGapInserterC' d12
    where
      header = EthernetHeader {
        _macDst = MacAddress $ repeat 0xFF,
        _macSrc = MacAddress $ repeat 0xFF,
        _etherType = 0xFFFF
      }
      asyncFifoC' = asyncFifoC d4 hasClock hasReset hasEnable ethClk ethRst ethEn
      downConverterC' = exposeClockResetEnable downConverterC ethClk ethRst ethEn
      interpacketGapInserterC' = exposeClockResetEnable interpacketGapInserterC ethClk ethRst ethEn
