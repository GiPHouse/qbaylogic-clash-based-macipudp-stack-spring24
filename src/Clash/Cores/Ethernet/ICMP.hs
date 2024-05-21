{-# language RecordWildCards #-}
{-|
Module      : Clash.Cores.Ethernet.ICMP
Description : Specialized function for parsing a partial ICMP header.
-}
module Clash.Cores.Ethernet.ICMP
  ( icmpReceiverC
    ) where

import Clash.Prelude
import Protocols

import Clash.Cores.Ethernet.Depacketizer
import Clash.Cores.Ethernet.EthernetTypes
import Clash.Cores.Ethernet.PacketStream


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
    f IcmpHeader{..} ipheader = (ipheader,  IcmpHeaderLite{_typeL = _type})
