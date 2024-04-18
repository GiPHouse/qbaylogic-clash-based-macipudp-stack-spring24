{-# language FlexibleContexts #-}
{-# language NumericUnderscores #-}

module Test.Cores.Ethernet.Packetizer
  (tests, packetizerModel) where

-- base
import Data.List qualified as L
import Prelude

-- tasty
import Test.Tasty
import Test.Tasty.Hedgehog ( HedgehogTestLimit(HedgehogTestLimit) )
import Test.Tasty.TH ( testGroupGenerator )

-- clash-prelude
import Clash.Prelude hiding ( concat )
import Clash.Prelude qualified as C

-- Me
import Clash.Cores.Ethernet.PacketStream

import Test.Cores.Ethernet.Util

-- | Model of the generic `packetizerC`.
packetizerModel
  :: forall (dataWidth :: Nat)
            (headerBytes :: Nat)
            (metaIn :: Type)
            (header :: Type)
            (metaOut :: Type) .
  ( KnownNat dataWidth
  , KnownNat headerBytes
  , 1 <= dataWidth
  , 1 <= headerBytes
  , BitPack header
  , BitSize header ~ headerBytes * 8)
  => (metaIn -> metaOut)
  -> (metaIn -> header)
  -> [PacketStreamM2S dataWidth metaIn]
  -> [PacketStreamM2S dataWidth metaOut]
packetizerModel toMetaOut toHeader ps = concat dataWidthPackets
  where
    metaOut = toMetaOut (_meta (L.head ps))
    header = toHeader (_meta (L.head ps))

    prependHdr :: [PacketStreamM2S 1 metaIn] -> [PacketStreamM2S 1 metaOut]
    prependHdr fragments = hdr L.++  L.map (\f -> f { _meta =  metaOut}) fragments
    
    bytes :: [BitVector 8]
    bytes = toList $ unpack $ pack header

    hdr :: [PacketStreamM2S 1 metaOut]
    hdr = L.map go bytes
      where
        go byte = PacketStreamM2S {
          _data = byte :> Nil,
          _last = Nothing,
          _meta = metaOut,
          _abort = False
        }

    bytePackets :: [[PacketStreamM2S 1 metaIn]]
    bytePackets = L.concatMap chopPacket . smearAbort <$> chunkByPacket ps

    prependedPackets :: [[PacketStreamM2S 1 metaOut]]
    prependedPackets = L.map prependHdr bytePackets

    dataWidthPackets :: [[PacketStreamM2S dataWidth metaOut]]
    dataWidthPackets = fmap chunkToPacket . chopBy (C.natToNum @dataWidth) <$> prependedPackets

tests :: TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1_000_000))
  $(testGroupGenerator)
