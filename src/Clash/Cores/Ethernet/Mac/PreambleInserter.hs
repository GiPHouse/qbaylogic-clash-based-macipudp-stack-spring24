{-|
Module      : Clash.Cores.Ethernet.Mac.PreambleInserter
Description : Inserts the ethernet preamble.
-}
module Clash.Cores.Ethernet.Mac.PreambleInserter
  (preambleInserterC) where

import Clash.Prelude

import Protocols

import Clash.Cores.Ethernet.Mac.EthernetTypes
import Protocols.Extra.PacketStream
import Protocols.Extra.PacketStream.Packetizer


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
