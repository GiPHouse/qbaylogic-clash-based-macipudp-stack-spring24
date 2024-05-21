{-|
Module      : Clash.Cores.Ethernet.ICMP
Description : ....
-}
module Clash.Cores.Ethernet.ICMP
  ( ) where

import Clash.Prelude
import Protocols

import Clash.Cores.Ethernet.EthernetTypes
import Clash.Cores.Ethernet.InternetChecksum
import Clash.Cores.Ethernet.Depacketizer
import Clash.Cores.Ethernet.PacketStream
import Data.Functor
import Data.Maybe

import Control.DeepSeq ( NFData )


data IcmpHeader = IcmpHeader {
  _type :: BitVector 8,
  _code :: BitVector 8,
  _checksum :: BitVector 16,
  _restofheader :: BitVector 32
} deriving (Show, ShowX, Eq, Generic, BitPack, NFDataX, NFData)

func :: (BitPack header) => header -> EthernetHeader -> (MacAddress, IcmpHeader)
func _ _ = undefined


icmpReceiverC
  :: HiddenClockResetEnable dom
  => Circuit (PacketStream dom n EthernetHeader) (PacketStream dom n (MacAddress, IcmpHeader))
icmpReceiverC = depacketizerC func