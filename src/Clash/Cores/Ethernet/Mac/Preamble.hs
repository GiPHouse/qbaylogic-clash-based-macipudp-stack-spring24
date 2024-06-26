{-|
Module      : Clash.Cores.Ethernet.Mac.Preamble
Description : Provides circuits to insert and strip the ethernet preamble.
-}
module Clash.Cores.Ethernet.Mac.Preamble
  ( preambleInserterC
  , preambleStripperC
  ) where

import Clash.Prelude

import Protocols
import Protocols.Extra.PacketStream
import Protocols.Extra.PacketStream.Packetizers

import Clash.Cores.Ethernet.Mac.EthernetTypes


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

-- | Strips the incoming PacketStream of the preamble and SFD. Drops a packet only
--   if the SFD is not correct, the circuit does not check if the preamble itself
--   matches for efficiency reasons.
preambleStripperC
  :: forall
     (dom :: Domain)
     (dataWidth :: Nat)
   . HiddenClockResetEnable dom
  => KnownNat dataWidth
  => 1 <= dataWidth
  => Circuit (PacketStream dom dataWidth ()) (PacketStream dom dataWidth ())
-- Only put the SFD in the metadata for efficiency reasons.
preambleStripperC =
  depacketizerC (\(p :: Preamble) _ -> last p)
  |> filterMeta (== startFrameDelimiter)
  |> mapMeta (const ())
