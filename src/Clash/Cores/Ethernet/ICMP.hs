{-# LANGUAGE RecordWildCards #-}
{-|
Module      : Clash.Cores.Ethernet.ICMP
Description : ....
-}
module Clash.Cores.Ethernet.ICMP
  ( icmpReceiverC,
    IcmpHeader
    ) where

import Clash.Prelude
import Protocols

import Clash.Cores.Ethernet.EthernetTypes
-- import Clash.Cores.Ethernet.InternetChecksum
import Clash.Cores.Ethernet.Depacketizer
import Clash.Cores.Ethernet.PacketStream
-- import Data.Functor
-- import Data.Maybe

import Control.DeepSeq ( NFData )


data IcmpHeader = IcmpHeader {
  _type :: BitVector 8,
  _code :: BitVector 8,
  _checksum :: BitVector 16,
  _restofheader :: BitVector 32
} deriving (Show, ShowX, Eq, Generic, BitPack, NFDataX, NFData)

newtype IcmpHeaderLite = IcmpHeaderLite {_typeL :: BitVector 8}
  deriving (Show, ShowX, Eq, Generic, BitPack, NFDataX, NFData)

func :: (BitPack header) => header -> IPv4HeaderLite -> (IPv4HeaderLite, IcmpHeaderLite)
func _ _ = undefined


icmpReceiverC :: forall (dom :: Domain) (dataWidth :: Nat).
  ( KnownDomain dom
  , HiddenClockResetEnable dom
  , KnownNat dataWidth
  , 1 <= dataWidth
  )
  => Circuit (PacketStream dom dataWidth IPv4HeaderLite) (PacketStream dom dataWidth (IPv4HeaderLite, IcmpHeaderLite))
icmpReceiverC = depacketizerC func