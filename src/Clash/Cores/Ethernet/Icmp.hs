{-# language RecordWildCards #-}

{-|
Module      : Clash.Cores.Ethernet.Icmp
Description : Top level ICMP module.
-}
module Clash.Cores.Ethernet.Icmp
  ( IcmpHeader(..)
  , IcmpHeaderLite(..)
  , icmpReceiverC
  , icmpTransmitterC
  , icmpEchoResponderC
  ) where

import Clash.Prelude

import Protocols ( Circuit, (|>) )
import Protocols.Extra.PacketStream
import Protocols.Extra.PacketStream.Packetizers ( depacketizerC, packetizerC )

import Clash.Cores.Ethernet.IP.InternetChecksum ( onesComplementAdd )
import Clash.Cores.Ethernet.IP.IPv4Types ( IPv4Address, IPv4HeaderLite(..) )

import Control.DeepSeq ( NFData )


-- | Full ICMP header
data IcmpHeader = IcmpHeader {
  _type :: BitVector 8,
  _code :: BitVector 8,
  _checksum :: BitVector 16
  } deriving (Show, ShowX, Eq, Generic, BitPack, NFDataX, NFData)

-- | Small ICMP header with only the type
data IcmpHeaderLite = IcmpHeaderLite {
  _typeL :: BitVector 8,
  _checksumL :: BitVector 16
  } deriving (Show, ShowX, Eq, Generic, BitPack, NFDataX, NFData)

icmpEchoResponderC ::
  forall (dom :: Domain) (dataWidth :: Nat).
  HiddenClockResetEnable dom
  => KnownNat dataWidth
  => 1 <= dataWidth
  => Signal dom IPv4Address
  -> Circuit (PacketStream dom dataWidth IPv4HeaderLite) (PacketStream dom dataWidth IPv4HeaderLite)
icmpEchoResponderC ourIP = icmpReceiverC |> mapMetaS (updateMeta <$> ourIP)  |> icmpTransmitterC

updateMeta ::
  IPv4Address
  -> (IPv4HeaderLite, IcmpHeaderLite)
  -> (IPv4HeaderLite, IcmpHeaderLite)
updateMeta ip (ipv4, icmp) = (adjustIP ip ipv4, adjustIcmp icmp)

adjustIP :: IPv4Address -> IPv4HeaderLite -> IPv4HeaderLite
adjustIP ip hdr@IPv4HeaderLite {..} = hdr {
  _ipv4lSource = ip
  , _ipv4lDestination = _ipv4lSource
}

adjustIcmp :: IcmpHeaderLite -> IcmpHeaderLite
adjustIcmp IcmpHeaderLite {..}  =
  IcmpHeaderLite { _typeL = 0 , _checksumL = onesComplementAdd (complement 0x0800) _checksumL }

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
