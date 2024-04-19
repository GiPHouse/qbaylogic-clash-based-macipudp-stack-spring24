module Clash.Cores.Ethernet.EthernetTypes
  (MacAddress, EthernetHeader, Preamble, startFrameDelimiter) where

import Clash.Prelude

import Control.DeepSeq ( NFData )


newtype MacAddress = MacAddress (Vec 6 (BitVector 8))
  deriving (Show, ShowX, Eq, Generic, BitPack, NFDataX, NFData)

data EthernetHeader = EthernetHeader {
  _macDst :: MacAddress,
  _macSrc :: MacAddress,
  _etherType :: BitVector 16
} deriving (Show, ShowX, Eq, Generic, BitPack, NFDataX, NFData)

type Preamble = Vec 8 (BitVector 8)

startFrameDelimiter :: BitVector 8
startFrameDelimiter = 0xD5
