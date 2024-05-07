{-# language FlexibleContexts #-}

module Test.Cores.Ethernet.Packetizer
  ( packetizerModel
  , packetizeFromDfModel) where

-- base
import Data.List qualified as L
import Prelude

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
packetizerModel toMetaOut toHeader ps = concat (upConvert <$> L.map prependHdr bytePackets)
  where
    prependHdr :: [PacketStreamM2S 1 metaIn] -> [PacketStreamM2S 1 metaOut]
    prependHdr fragments = hdr L.++ L.map (\f -> f { _meta = metaOut}) fragments
      where
        metaOut = toMetaOut (_meta (L.head fragments))
        header = toHeader (_meta (L.head fragments))
        hdr = L.map go (toList $ bitCoerce header)
          where
            go byte = PacketStreamM2S {
              _data = byte :> Nil,
              _last = Nothing,
              _meta = metaOut,
              _abort = _abort (L.head fragments)
            }

    bytePackets :: [[PacketStreamM2S 1 metaIn]]
    bytePackets = L.concatMap chopPacket . smearAbort <$> chunkByPacket ps

-- | Model of the generic `packetizeFromDfC`.
packetizeFromDfModel
  :: forall (dataWidth :: Nat)
            (headerBytes :: Nat)
            (a :: Type)
            (header :: Type)
            (metaOut :: Type) .
  ( KnownNat dataWidth
  , KnownNat headerBytes
  , 1 <= dataWidth
  , 1 <= headerBytes
  , BitPack header
  , BitSize header ~ headerBytes * 8)
  => (a -> metaOut)
  -> (a -> header)
  -> [a]
  -> [PacketStreamM2S dataWidth metaOut]
packetizeFromDfModel toMetaOut toHeader ps = concat (upConvert <$> L.map packetize ps)
  where
    packetize :: a -> [PacketStreamM2S 1 metaOut]
    packetize d = fullPackets $ L.map (\byte -> PacketStreamM2S (byte :> Nil) Nothing (toMetaOut d) False)
                                      (toList $ bitCoerce (toHeader d))
