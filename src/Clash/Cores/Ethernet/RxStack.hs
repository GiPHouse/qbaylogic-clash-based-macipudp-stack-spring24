module Clash.Cores.Ethernet.RxStack
( rxStack
) where

import Clash.Cores.Ethernet.PacketStream
import Clash.Prelude
import Protocols
import Clash.Cores.Ethernet.AsyncFIFO (asyncFifoC)
import Clash.Cores.Ethernet.UpConverter (upConverterC)

-- For now this is just an Id circuit,
-- Will be replaced with everything that has to be done
-- for an ethernet stack in the future.
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