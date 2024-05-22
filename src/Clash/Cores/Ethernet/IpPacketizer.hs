{-|
Module      : Clash.Cores.Ethernet.IpPacketizer
Description : Specialized packetizer for IP header
-}
module Clash.Cores.Ethernet.IpPacketizer
  ( ipPacketizerC
  , ipLitePacketizerC
  ) where

import Clash.Prelude
import Protocols

import Clash.Cores.Ethernet.EthernetTypes
import Clash.Cores.Ethernet.InternetChecksum
import Clash.Cores.Ethernet.Packetizer
import Clash.Cores.Ethernet.PacketStream
import Data.Functor
import Data.Maybe

-- | Packetize a packet stream with the IPv4HeaderLite meta data
-- giving default values for header data that are not in IPv4HeaderLite.
ipLitePacketizerC
  :: forall (dom :: Domain)
            (dataWidth :: Nat) .
  ( HiddenClockResetEnable dom
  , KnownDomain dom
  , 1 <= dataWidth
  , KnownNat dataWidth)
  => Circuit (PacketStream dom dataWidth IPv4HeaderLite) (PacketStream dom dataWidth IPv4Address)
ipLitePacketizerC = fromLiteC |> ipPacketizerC

-- | Packetize a packet stream with the IPv4Header meta data.
ipPacketizerC
  :: forall (dom :: Domain)
            (dataWidth :: Nat)
   . ( HiddenClockResetEnable dom
     , KnownDomain dom
     , 1 <= dataWidth
     , KnownNat dataWidth
     )
  => Circuit (PacketStream dom dataWidth IPv4Header) (PacketStream dom dataWidth IPv4Address)
ipPacketizerC = setChecksumC |> packetizerC _ipv4Destination id

-- | Internal state of `setChecksumC`
data ChecksumS
  = Wait    -- ^ Waiting for new packet
  | Compute -- ^ Computing checksum
  | Forward (BitVector 16) -- ^ Forwarding data
  deriving (Eq, Generic, NFDataX)

-- | Set the checksum in the IPv4Header of the metatype
setChecksumC
  :: forall dom dataWidth
   . HiddenClockResetEnable dom
  => KnownDomain dom
  => Circuit (PacketStream dom dataWidth IPv4Header) (PacketStream dom dataWidth IPv4Header)
setChecksumC = Circuit $ \(fwdInS, bwdInS) ->
  let
    s = register Wait s'
    (s', unbundle -> (bwdOutS, fwdOutS)) = unbundle $ bundle (s, fwdInS, bwdInS, counter, checksum) <&> \case
      (Wait, Just _, _, _, _)         -> (Compute, stall)
      (Wait, _, _, _, _)              -> (Wait, stall)
      (Compute, fwdIn, bwdIn, 0, c)   -> go c fwdIn bwdIn
      (Compute, _, _, _, _)           -> (Compute, stall)
      (Forward c, fwdIn, bwdIn, _, _) -> go c fwdIn bwdIn

    go c fwdIn bwdIn = (s'', (bwdIn, replaceChecksum c fwdIn))
      where
        s'' | isJust fwdIn && isJust (_last (fromJustX fwdIn)) = Wait
            | otherwise = Forward c

    stall = (PacketStreamS2M False, Nothing)
    replaceChecksum c mp = ((\h -> h {_ipv4Checksum = c}) <$>) <$> mp

    -- Calculating the checksum
    replaceBuffer = (s .==. pure Wait) .&&. isJust <$> fwdInS
    ipHeader = bitCoerce . _meta . fromJustX <$> fwdInS
    buffer :: Signal dom (Vec 10 (BitVector 16))
    counter :: Signal dom (Index 11)
    buffer = register (ensureSpine defaultBytes) (mux replaceBuffer ipHeader ((<<+ defaultBytes) <$> buffer))
    counter = register 0 $ mux replaceBuffer 10 (satPred SatBound <$> counter)
    checksum = complement <$> internetChecksum (liftA2 (\x b -> Just (head x, b)) buffer replaceBuffer)

    defaultBytes = errorX "ipPacketizerC: undefined value in header register"
   in (bwdOutS, fwdOutS)
