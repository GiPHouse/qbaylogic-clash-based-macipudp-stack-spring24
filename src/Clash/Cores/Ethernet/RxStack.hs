module Clash.Cores.Ethernet.RxStack
( rxStack
) where

import Clash.Cores.Ethernet.AsyncFIFO ( asyncFifoC )
import Clash.Cores.Ethernet.PacketStream
import Clash.Cores.Ethernet.PreambleStripper ( preambleStripperC )
import Clash.Cores.Ethernet.UpConverter ( upConverterC )
import Clash.Prelude
import Protocols


rxStack
  :: forall
  (dataWidth :: Nat)
  (dom :: Domain)
  (domEth :: Domain).
  ( HiddenClockResetEnable dom
  , KnownDomain domEth
  )
  => 1 <= dataWidth
  => KnownNat dataWidth
  => Clock domEth
  -> Circuit (PacketStream domEth 1 ()) (PacketStream dom dataWidth ())
rxStack ethClk = stack
  where
    upConverterC' = exposeClockResetEnable upConverterC ethClk resetGen enableGen
    stack = upConverterC'
             |> asyncFifoC d4 ethClk resetGen enableGen hasClock hasReset hasEnable
             |> preambleStripperC
