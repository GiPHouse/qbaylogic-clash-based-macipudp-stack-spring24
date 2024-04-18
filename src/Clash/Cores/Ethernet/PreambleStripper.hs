module Clash.Cores.Ethernet.PreambleStripper where

import Clash.Prelude

import Protocols

import Clash.Cores.Ethernet.PacketStream
import Clash.Cores.Ethernet.Depacketizer
import Clash.Cores.Ethernet.EthernetTypes


startFrameDelimiter :: BitVector 8
startFrameDelimiter = 0xD5

-- | Strips the incoming PacketStream of the preamble and SFD. Drops a packet only if the SFD is not correct,
--   it does not check if the preamble itself matches for efficiency reasons.
preambleStripperC :: forall (dom :: Domain) (dataWidth :: Nat).
  ( KnownDomain dom
  , HiddenClockResetEnable dom
  , KnownNat dataWidth
  , 1 <= dataWidth
  )
  => Circuit (PacketStream dom dataWidth ()) (PacketStream dom dataWidth ())
preambleStripperC = depacketizePreamble |> forceResetSanity |> fromSignals ckt
  where
    depacketizePreamble :: Circuit (PacketStream dom dataWidth ()) (PacketStream dom dataWidth Preamble)
    depacketizePreamble = depacketizerC const

    ckt (fwdIn, bwdIn) = (bwdOut, fwdOut)
      where
        bwdOut = bwdIn
        fwdOut = fmap go fwdIn
        go f = case f of
          Nothing -> Nothing
          Just p -> if last (_meta p) == startFrameDelimiter
                    then Just (p {_meta = ()})
                    -- Drop the packet
                    else Nothing
