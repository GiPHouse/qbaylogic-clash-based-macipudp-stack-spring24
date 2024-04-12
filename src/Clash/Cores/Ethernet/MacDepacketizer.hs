module Clash.Cores.Ethernet.MacDepacketizer
  ( macDepacketizerC
  , MacAddress
  , EthernetHeader) where

import Clash.Prelude

import Protocols

import Clash.Cores.Ethernet.PacketStream
import Clash.Cores.Ethernet.Depacketizer

import Control.DeepSeq (NFData)


newtype MacAddress = MacAddress (Vec 6 (BitVector 8))
  deriving (Show, ShowX, Eq, Generic, BitPack, NFDataX, NFData)

data EthernetHeader = EthernetHeader {
  _mac_dst :: MacAddress,
  _mac_src :: MacAddress,
  _ether_type :: BitVector 16
} deriving (Show, ShowX, Eq, Generic, BitPack, NFDataX, NFData)

-- | Parses the first 14 bytes of the incoming PacketStream into an `EthernetHeader`.
macDepacketizerC :: forall (dom :: Domain) (dataWidth :: Nat) (m :: Nat) .
  ( KnownDomain dom
  , HiddenClockResetEnable dom
  , KnownNat dataWidth
  , 1 <= dataWidth
  , Mod 14 dataWidth + m ~ dataWidth)
  => SNat dataWidth
  -> Circuit (PacketStream dom dataWidth ()) (PacketStream dom dataWidth EthernetHeader)
macDepacketizerC dataWidth = depacketizerC dataWidth d14
