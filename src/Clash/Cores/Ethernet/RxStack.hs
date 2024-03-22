module Clash.Cores.Ethernet.RxStack
( rxStack
) where

import Clash.Prelude
    ( KnownNat, Nat, Domain, type (<=), HiddenClockResetEnable )
import Protocols
import Clash.Cores.Ethernet.PacketStream

-- For now this is just an Id circuit,
-- Will be replaced with everything that has to be done
-- for an ethernet stack in the future.
rxStack :: forall (dataWidth :: Nat) (dom :: Domain).
  HiddenClockResetEnable dom
  => 1 <= dataWidth
  => KnownNat dataWidth
  => Circuit (PacketStream dom dataWidth ()) (PacketStream dom dataWidth ())
rxStack = idC