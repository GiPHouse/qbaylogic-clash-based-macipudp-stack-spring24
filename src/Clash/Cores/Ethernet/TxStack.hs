module Clash.Cores.Ethernet.TxStack
  ( txStack
  ) where

import Clash.Cores.Ethernet.AsyncFIFO ( asyncFifoC )
import Clash.Cores.Ethernet.DownConverter
import Clash.Cores.Ethernet.InterpacketGapInserter ( interpacketGapInserterC )
import Clash.Cores.Ethernet.PacketStream
import Clash.Prelude
import Protocols

txStack
  :: forall (dataWidth :: Nat) (dom :: Domain) (domEth :: Domain)
   . ( KnownNat dataWidth
     , 1 <= dataWidth
     , HiddenClockResetEnable dom
     , KnownDomain domEth
     )
  => Clock domEth
  -> Reset domEth
  -> Enable domEth
  -> Circuit (PacketStream dom dataWidth ()) (PacketStream domEth 1 ())
txStack ethClk ethRst ethEn = asyncFifoC' |> downConverterC' |> interpacketGapInserterC' d12
  where
    asyncFifoC' = asyncFifoC d4 hasClock hasReset hasEnable ethClk ethRst ethEn
    downConverterC' = exposeClockResetEnable downConverterC ethClk ethRst ethEn
    interpacketGapInserterC' = exposeClockResetEnable interpacketGapInserterC ethClk ethRst ethEn
