{-# language FlexibleContexts #-}
{-# language NumericUnderscores #-}
{-# language RecordWildCards #-}

module Test.Cores.Ethernet.IP.IPPacketizers where

import Control.Monad
import Prelude

import Clash.Prelude qualified as C
import Clash.Sized.Vector qualified as C

import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Test.Tasty
import Test.Tasty.Hedgehog ( HedgehogTestLimit(HedgehogTestLimit) )
import Test.Tasty.Hedgehog.Extra ( testProperty )
import Test.Tasty.TH ( testGroupGenerator )

import Protocols.Extra.PacketStream
import Protocols.Hedgehog

import Clash.Cores.Ethernet.IP.IPPacketizers ( ipDepacketizerC, ipPacketizerC )
import Clash.Cores.Ethernet.IP.IPv4Types
import Clash.Cores.Ethernet.Mac.EthernetTypes

import Test.Cores.Ethernet.IP.InternetChecksum ( pureInternetChecksum )
import Test.Cores.Ethernet.Util
import Test.Protocols.Extra.PacketStream.Depacketizer ( depacketizerModel )
import Test.Protocols.Extra.PacketStream.Packetizer ( packetizerModel )


genVec :: (C.KnownNat n, 1 C.<= n) => Gen a -> Gen (C.Vec n a)
genVec gen = sequence (C.repeat gen)

genPackets :: 1 C.<= n => C.KnownNat n => Gen a -> Gen (PacketStreamM2S n a)
genPackets genMeta =
  PacketStreamM2S <$>
  genVec Gen.enumBounded <*>
  Gen.maybe Gen.enumBounded <*>
  genMeta <*>
  Gen.enumBounded

-- | Tests the IP depacketizer for arbitrary packets
testSetChecksumC
  :: forall (dataWidth :: C.Nat)
   . ( C.KnownNat dataWidth
     , 1 C.<= dataWidth
     , 20 `C.Mod` dataWidth C.<= dataWidth
     )
  => C.SNat dataWidth
  -> Property
testSetChecksumC _ = idWithModelSingleDomain
  @C.System defExpectOptions
  gen
  (C.exposeClockResetEnable $ packetizerModel _ipv4Destination id . model)
  (C.exposeClockResetEnable (ipPacketizerC @C.System @dataWidth))
  where
    fragments = fmap fullPackets (Gen.list (Range.linear 1 100) (genPackets (pure ())))

    gen = do
      packets <- chunkByPacket <$> fragments
      headers <- replicateM (length packets) genIPv4Header
      return $ concat $ zipWith (\ps h -> (h <$) <$> ps) packets headers

    geb :: forall x . (Enum x, Bounded x) => Gen x
    geb = Gen.enumBounded
    genIpAddr = IPv4Address <$> genVec geb
    genIPv4Header = IPv4Header
      <$> pure 4 <*> pure 5 <*> geb <*> geb   --version, ihl, dscp, ecn
      <*> geb <*> geb <*> geb <*> geb <*> geb --length, identification, flags
      <*> geb <*> geb <*> geb <*> pure 0      --fragment-offset, protocol, ttl, checksum
      <*> genIpAddr <*> genIpAddr             --source, destination

    model ps = withChecksum
      where
        checksums = pureInternetChecksum @(C.Vec 10) . C.bitCoerce . _meta <$> ps
        withChecksum = zipWith (\c p -> p {_meta = (_meta p) {_ipv4Checksum = c}}) checksums ps

-- Odd data widths
prop_ip_set_checksum_d8 :: Property
prop_ip_set_checksum_d8 = testSetChecksumC C.d8

-- | Tests the IP depacketizer for arbitrary packets
testIPDepacketizer
  :: forall (dataWidth :: C.Nat)
   . ( C.KnownNat dataWidth
     , 1 C.<= dataWidth
     , 20 `C.Mod` dataWidth C.<= dataWidth
     )
  => C.SNat dataWidth
  -> Property
testIPDepacketizer _ = idWithModelSingleDomain
  @C.System defExpectOptions
  gen
  (C.exposeClockResetEnable model)
  (C.exposeClockResetEnable (ipDepacketizerC @C.System @dataWidth))
  where
    gen = Gen.choice [genGarbage, genValidHeaders]

    genGarbage = fmap fullPackets (Gen.list (Range.linear 1 100) (genPackets (C.unpack <$> Gen.enumBounded)))
    geb :: forall x . (Enum x, Bounded x) => Gen x
    geb = Gen.enumBounded
    genIpAddr = IPv4Address <$> genVec geb
    genIPv4Header = IPv4Header <$> pure 4 <*> pure 5 <*> geb <*> geb <*> geb <*> geb <*> geb <*> geb <*> geb <*> geb <*> geb <*> geb <*> pure 0 <*> genIpAddr <*> genIpAddr
    genValidHeaderPacket = do
      ethernetHeader :: EthernetHeader <- C.unpack <$> Gen.enumBounded
      rawHeader <- genIPv4Header
      let checksum = pureInternetChecksum (C.bitCoerce rawHeader :: C.Vec 10 (C.BitVector 16))
          header = rawHeader {_ipv4Checksum = checksum}
          headerBytes = C.toList (C.bitCoerce header :: C.Vec 20 (C.BitVector 8))
      dataBytes :: [C.BitVector 8] <- Gen.list (Range.linear 0 (5 * C.natToNum @dataWidth)) geb
      let dataFragments = chopBy (C.natToNum @dataWidth) (headerBytes ++ dataBytes)
          fragments = (\x -> PacketStreamM2S x Nothing ethernetHeader False) <$> (C.unsafeFromList @dataWidth <$> (++ repeat 0xAA) <$> dataFragments)
          fragments' = init fragments ++ [(last fragments) {_last = Just (fromIntegral (length (last dataFragments) - 1))}]
      aborts <- sequence (Gen.bool <$ fragments)
      let fragments'' = zipWith (\p abort -> p {_abort = abort}) fragments' aborts
      return fragments''

    genValidHeaders = concat <$> Gen.list (Range.linear 1 50) genValidHeaderPacket

    model ps =
     let
       ps' = depacketizerModel const ps
       aborts = (\h ->
         pureInternetChecksum (C.bitCoerce h :: C.Vec 10 (C.BitVector 16)) /= 0 ||
         _ipv4Ihl h /= 5 ||
         _ipv4Version h /= 4 ||
         _ipv4FlagReserved h ||
         _ipv4FlagMF h || _ipv4FragmentOffset h /= 0
         ) . _meta <$> ps'
      in
       concat $ zipWith (\qs abort -> (\q -> q {_abort = _abort q || abort}) <$> qs) (chunkByPacket ps') (aborts ++ repeat False)

-- Odd data widths
prop_ip_depacketizer_d1 :: Property
prop_ip_depacketizer_d1 = testIPDepacketizer C.d1

prop_ip_depacketizer_d7 :: Property
prop_ip_depacketizer_d7 = testIPDepacketizer C.d7

prop_ip_depacketizer_d19 :: Property
prop_ip_depacketizer_d19 = testIPDepacketizer C.d19

prop_ip_depacketizer_d21 :: Property
prop_ip_depacketizer_d21 = testIPDepacketizer C.d21

prop_ip_depacketizer_d23 :: Property
prop_ip_depacketizer_d23 = testIPDepacketizer C.d23

-- Even data widths
prop_ip_depacketizer_d2 :: Property
prop_ip_depacketizer_d2 = testIPDepacketizer C.d2

prop_ip_depacketizer_d6 :: Property
prop_ip_depacketizer_d6 = testIPDepacketizer C.d6

prop_ip_depacketizer_d18 :: Property
prop_ip_depacketizer_d18 = testIPDepacketizer C.d18

prop_ip_depacketizer_d20 :: Property
prop_ip_depacketizer_d20 = testIPDepacketizer C.d20

prop_ip_depacketizer_d28 :: Property
prop_ip_depacketizer_d28 = testIPDepacketizer C.d28

tests :: TestTree
tests =
    localOption (mkTimeout 20_000_000 {- 20 seconds -})
  $ localOption (HedgehogTestLimit (Just 1_000))
  $(testGroupGenerator)
