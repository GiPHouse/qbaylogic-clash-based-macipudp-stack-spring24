{-|
Module      : Clash.Cores.Ethernet.PreambleStripper
Description : Provides @preambleStripperC@ which strips the Ethernet preamble from an incoming packet stream and validates it.
-}
module Clash.Cores.Ethernet.PreambleStripper
  ( preambleStripperC
  ) where

import Clash.Prelude

import Protocols

import Clash.Cores.Ethernet.Depacketizer
import Clash.Cores.Ethernet.EthernetTypes
import Clash.Cores.Ethernet.PacketStream


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
