{-|
Module      : Clash.Cores.Ethernet.Mac.PreambleStripper
Description : Provides @preambleStripperC@ which strips the Ethernet preamble from an incoming packet stream and validates it.
-}
module Clash.Cores.Ethernet.Mac.PreambleStripper
  ( preambleStripperC
  ) where

import Clash.Prelude

import Protocols

import Clash.Cores.Ethernet.Mac.EthernetTypes
import Protocols.Extra.PacketStream
import Protocols.Extra.PacketStream.Depacketizer


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
