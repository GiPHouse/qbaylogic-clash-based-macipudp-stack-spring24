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
import Clash.Cores.Ethernet.Util

import Data.Maybe (isNothing)

-- | Strips the incoming PacketStream of the preamble and SFD. Drops a packet only if the SFD is not correct,
--   the circuit does not check if the preamble itself matches for efficiency reasons.
preambleStripperC
  :: forall (dom :: Domain) (dataWidth :: Nat)
   . ( KnownDomain dom
     , HiddenClockResetEnable dom
     , KnownNat dataWidth
     , 1 <= dataWidth
     )
  => Circuit (PacketStream dom dataWidth ()) (PacketStream dom dataWidth ())
-- Only put the SFD in the metadata for efficiency reasons.
preambleStripperC = depacketizerC (\p _ -> last (p :: Preamble)) |> fromSignals ckt
 where
  ckt (fwdIn, bwdIn) = (bwdOut, fwdOut)
   where
    -- It's illegal to look at bwdIn when you are sending out a Nothing
    -- since the test framework drives undefined acks randomly if fwdOut
    -- is Nothing. So if we drive a Nothing force a True on bwdOut.
    bwdOut = PacketStreamS2M <$> ((isNothing <$> fwdOut) .||. _ready <$> bwdIn)
    -- We send Nothing if the SFD is not 0xD5. That is, we drop the packet.
    dropBadSfd p = toMaybe (_meta p == startFrameDelimiter) (p{_meta = ()})
    fwdOut = (dropBadSfd =<<) <$> fwdIn
