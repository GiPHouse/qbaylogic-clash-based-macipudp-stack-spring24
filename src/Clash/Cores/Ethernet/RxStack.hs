module Clash.Cores.Ethernet.RxStack
( rxStack
) where

import Clash.Cores.Ethernet.PacketStream
import Clash.Prelude ( Domain, HiddenClockResetEnable, KnownNat, Nat, type (<=) )
import Protocols

-- For now this is just an Id circuit,
-- Will be replaced with everything that has to be done
-- for an ethernet stack in the future.
rxStack :: forall (dataWidth :: Nat) (dom :: Domain).
  HiddenClockResetEnable dom
  => 1 <= dataWidth
  => KnownNat dataWidth
  => Circuit (PacketStream dom dataWidth ()) (PacketStream dom dataWidth ())
rxStack = idC
