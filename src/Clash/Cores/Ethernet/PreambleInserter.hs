{-|
Module      : Clash.Cores.Ethernet.PreambleInserter
Description : Inserts the ethernet preamble.
-}
module Clash.Cores.Ethernet.PreambleInserter
  (preambleInserterC) where

import Clash.Prelude

import Protocols

import Clash.Cores.Ethernet.EthernetTypes
import Clash.Cores.Ethernet.Packetizer
import Clash.Cores.Ethernet.PacketStream


-- | Prepends the ethernet preamble to the packet stream, for each individual packet.
preambleInserterC
  :: forall (dom :: Domain)
            (dataWidth :: Nat) .
  ( HiddenClockResetEnable dom
  , KnownDomain dom
  , 1 <= dataWidth
  , KnownNat dataWidth)
  => Circuit (PacketStream dom dataWidth ()) (PacketStream dom dataWidth ())
preambleInserterC = packetizerC (const ()) (const preamble)
