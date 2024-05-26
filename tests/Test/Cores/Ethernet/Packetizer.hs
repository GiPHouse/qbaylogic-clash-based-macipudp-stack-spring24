{-# language FlexibleContexts #-}

module Test.Cores.Ethernet.Packetizer
  ( packetizerModel
  , packetizeFromDfModel
  ) where

import Data.List qualified as L
import Prelude

import Clash.Prelude

import Protocols.Extra.PacketStream

import Test.Cores.Ethernet.Util


-- | Model of the generic `packetizerC`.
packetizerModel
  :: forall (dataWidth :: Nat)
            (headerBytes :: Nat)
            (metaIn :: Type)
            (header :: Type)
            (metaOut :: Type)
   . KnownNat dataWidth
  => KnownNat headerBytes
  => 1 <= dataWidth
  => 1 <= headerBytes
  => BitPack header
  => BitSize header ~ headerBytes * 8
  => (metaIn -> metaOut)
  -> (metaIn -> header)
  -> [PacketStreamM2S dataWidth metaIn]
  -> [PacketStreamM2S dataWidth metaOut]
packetizerModel toMetaOut toHeader ps = L.concatMap (upConvert . prependHdr) bytePackets
  where
    prependHdr :: [PacketStreamM2S 1 metaIn] -> [PacketStreamM2S 1 metaOut]
    prependHdr fragments = hdr L.++ L.map (\f -> f { _meta = metaOut}) fragments
      where
        h = L.head fragments
        metaOut = toMetaOut (_meta h)
        hdr = L.map go (toList $ bitCoerce (toHeader (_meta h)))
        go byte = PacketStreamM2S (singleton byte) Nothing metaOut (_abort h)

    bytePackets :: [[PacketStreamM2S 1 metaIn]]
    bytePackets = downConvert . smearAbort <$> chunkByPacket ps

-- | Model of the generic `packetizeFromDfC`.
packetizeFromDfModel
  :: forall (dataWidth :: Nat)
            (headerBytes :: Nat)
            (a :: Type)
            (header :: Type)
            (metaOut :: Type)
   . KnownNat dataWidth
  => KnownNat headerBytes
  => 1 <= dataWidth
  => 1 <= headerBytes
  => BitPack header
  => BitSize header ~ headerBytes * 8
  => (a -> metaOut)
  -> (a -> header)
  -> [a]
  -> [PacketStreamM2S dataWidth metaOut]
packetizeFromDfModel toMetaOut toHeader = L.concatMap (upConvert . packetize)
  where
    packetize :: a -> [PacketStreamM2S 1 metaOut]
    packetize d = fullPackets $
      L.map (\byte -> PacketStreamM2S (byte :> Nil) Nothing (toMetaOut d) False)
            (toList $ bitCoerce (toHeader d))
