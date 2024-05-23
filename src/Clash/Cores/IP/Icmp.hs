{-# language RecordWildCards #-}
module Clash.Cores.IP.Icmp
  ( icmpTransmitterC
  , icmpReceiverC
  )
  where

import Clash.Prelude

import Clash.Cores.Ethernet.Depacketizer ( depacketizerC )
import Clash.Cores.Ethernet.Packetizer ( packetizerC )
import Clash.Cores.Ethernet.PacketStream
import Clash.Cores.IP.IcmpTypes
import Clash.Cores.IP.IPv4Types ( IPv4HeaderLite )
import Protocols ( Circuit )

icmpTransmitterC ::
  forall (dom::Domain) (n::Nat).
  HiddenClockResetEnable dom
  => KnownNat n
  => 1 <= n
  => Circuit (PacketStream dom n (IPv4HeaderLite, IcmpHeaderLite)) (PacketStream dom n IPv4HeaderLite)
icmpTransmitterC = packetizerC fst snd

-- | A circuit that parses an ICMP Header and output an IcmpHeaderLite
icmpReceiverC :: forall (dom :: Domain) (dataWidth :: Nat).
  ( KnownDomain dom
  , HiddenClockResetEnable dom
  , KnownNat dataWidth
  , 1 <= dataWidth
  )
  => Circuit (PacketStream dom dataWidth IPv4HeaderLite) (PacketStream dom dataWidth (IPv4HeaderLite, IcmpHeaderLite))
icmpReceiverC = depacketizerC f
  where
    f :: IcmpHeader -> IPv4HeaderLite -> (IPv4HeaderLite, IcmpHeaderLite)
    f IcmpHeader{..} ipheader = (ipheader,  IcmpHeaderLite{_typeL = _type, _checksumL = _checksum})
