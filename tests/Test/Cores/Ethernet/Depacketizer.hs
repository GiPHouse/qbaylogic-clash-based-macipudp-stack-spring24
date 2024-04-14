{-# language FlexibleContexts #-}

module Test.Cores.Ethernet.Depacketizer
  (depacketizerModel) where

-- base
import Data.List qualified as L
import Prelude

-- clash-prelude
import Clash.Prelude hiding ( concat )
import Clash.Prelude qualified as C
import Clash.Sized.Vector (unsafeFromList)

-- Me
import Clash.Cores.Ethernet.PacketStream

import Test.Cores.Ethernet.Util

-- | Model of the generic `depacketizerC`.
depacketizerModel
  :: forall (dataWidth :: Nat)
            (headerBytes :: Nat)
            (metaIn :: Type)
            (metaOut :: Type)
            (header :: Type) .
  ( KnownNat dataWidth
  , KnownNat headerBytes
  , 1 <= dataWidth
  , 1 <= headerBytes
  , BitPack header
  , BitSize header ~ headerBytes * 8)
  => (header -> metaIn -> metaOut)
  -> [PacketStreamM2S dataWidth metaIn]
  -> [PacketStreamM2S dataWidth metaOut]
depacketizerModel toMetaOut ps = concat dataWidthPackets
  where
    hdrbytes = natToNum @headerBytes

    parseHdr :: ([PacketStreamM2S 1 metaIn], [PacketStreamM2S 1 metaIn]) -> [PacketStreamM2S 1 metaOut]
    parseHdr (hdrF, fwdF) = fmap (\f -> f { _meta = metaOut }) fwdF
      where
        hdr = bitCoerce $ unsafeFromList @headerBytes $ _data <$> hdrF
        metaOut = toMetaOut hdr (_meta $ L.head fwdF)

    bytePackets :: [[PacketStreamM2S 1 metaIn]]
    bytePackets = L.filter (\fs -> L.length fs > hdrbytes)
                    $ L.concat . fmap chopPacket . smearAbort <$> chunkByPacket ps

    parsedPackets :: [[PacketStreamM2S 1 metaOut]]
    parsedPackets = parseHdr . L.splitAt hdrbytes <$> bytePackets

    dataWidthPackets :: [[PacketStreamM2S dataWidth metaOut]]
    dataWidthPackets = fmap chunkToPacket . chopBy (C.natToNum @dataWidth) <$> parsedPackets
