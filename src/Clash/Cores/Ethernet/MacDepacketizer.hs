module Clash.Cores.Ethernet.MacDepacketizer
  ( macDepacketizerC
  , MacAddress
  , EthernetHeader) where

import Clash.Prelude

import Protocols

import Clash.Cores.Ethernet.Depacketizer
import Clash.Cores.Ethernet.PacketStream

import Control.DeepSeq ( NFData )

newtype MacAddress = MacAddress (Vec 6 (BitVector 8))
  deriving (Show, ShowX, Eq, Generic, BitPack, NFDataX, NFData)

data EthernetHeader = EthernetHeader {
  _macDst :: MacAddress,
  _macSrc :: MacAddress,
  _etherType :: BitVector 16
} deriving (Show, ShowX, Eq, Generic, BitPack, NFDataX, NFData)

-- | Parses the first 14 bytes of the incoming PacketStream into an `EthernetHeader`.
macDepacketizerC :: forall (dom :: Domain) (dataWidth :: Nat).
  ( KnownDomain dom
  , HiddenClockResetEnable dom
  , KnownNat dataWidth
  , 1 <= dataWidth
  )
  => Circuit (PacketStream dom dataWidth ()) (PacketStream dom dataWidth EthernetHeader)
macDepacketizerC = depacketizerC const
