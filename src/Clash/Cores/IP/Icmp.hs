module Clash.Cores.IP.Icmp
  (
    icmpTransmitterC
  )
  where

import Clash.Prelude

import Clash.Cores.Ethernet.Packetizer ( packetizerC )
import Clash.Cores.Ethernet.PacketStream
import Clash.Cores.IP.IcmpTypes ( IcmpHeaderLite )
import Clash.Cores.IP.IPv4Types ( IPv4HeaderLite )
import Protocols ( Circuit )

icmpTransmitterC ::
  forall (dom::Domain) (n::Nat).
  HiddenClockResetEnable dom
  => KnownNat n
  => 1 <= n
  => Circuit (PacketStream dom n (IPv4HeaderLite, IcmpHeaderLite)) (PacketStream dom n IPv4HeaderLite)
icmpTransmitterC = packetizerC fst snd
