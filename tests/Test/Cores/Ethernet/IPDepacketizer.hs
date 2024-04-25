{-# language FlexibleContexts #-}
{-# language NumericUnderscores #-}
{-# language RecordWildCards #-}

module Test.Cores.Ethernet.IPDepacketizer where

-- base
import Prelude
import Data.Maybe
import Control.Monad (replicateM)

-- clash-prelude
import Clash.Prelude qualified as C

-- hedgehog
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

-- tasty
import Test.Tasty
import Test.Tasty.Hedgehog ( HedgehogTestLimit(HedgehogTestLimit) )
import Test.Tasty.Hedgehog.Extra ( testProperty )
import Test.Tasty.TH ( testGroupGenerator )

-- clash-protocols
import Protocols.Hedgehog

-- Me
import Clash.Cores.Ethernet.EthernetTypes
import Clash.Cores.Ethernet.PacketStream
import Clash.Cores.Ethernet.IPDepacketizer

import Test.Cores.Ethernet.Depacketizer ( depacketizerModel )
import Test.Cores.Ethernet.Util

import Test.Cores.Ethernet.InternetChecksum ( pureInternetChecksum )

genVec :: (C.KnownNat n, 1 C.<= n) => Gen a -> Gen (C.Vec n a)
genVec gen = sequence (C.repeat gen)

genPackets :: 1 C.<= n => C.KnownNat n => Gen a -> Gen (PacketStreamM2S n a)
genPackets genMeta =
  PacketStreamM2S <$>
  genVec Gen.enumBounded <*>
  Gen.maybe Gen.enumBounded <*>
  genMeta <*>
  Gen.enumBounded

-- | Tests the IP depacketizer with only valid IP headers.
testIPPacketizerValid
  :: forall (dataWidth :: C.Nat) k
   . ( C.KnownNat dataWidth
     , 1 C.<= dataWidth
     , dataWidth ~ 2 C.* k
     )
  => C.SNat dataWidth
  -> Property
testIPPacketizerValid _ = idWithModelSingleDomain
  @C.System defExpectOptions
  (upConvert <$> packetList)
  (C.exposeClockResetEnable model)
  (C.exposeClockResetEnable (ipDepacketizerC @C.System @dataWidth))
  where
    geb :: forall (a :: C.Type) . (Enum a, Bounded a) => Gen a
    geb = Gen.enumBounded
    randomHeader = IPv4Header <$> geb <*> pure 5 <*> geb <*> geb <*> geb <*> geb <*> geb <*> geb <*> geb <*> geb <*> pure 0 <*> geb <*> geb
    validHeader = (\h -> h {_ipv4Checksum = pureInternetChecksum (C.bitCoerce h :: C.Vec 10 (C.BitVector 16))}) <$> randomHeader
    headerPacket = PacketStreamM2S <$> (C.bitCoerce <$> validHeader) <*> pure Nothing <*> (C.unpack <$> Gen.enumBounded) <*> pure False
    dataPackets = fullPackets <$> Gen.list (Range.linear 0 5) (genPackets @20 (C.unpack <$> geb))
    packet = concatMap chopPacket <$> ((\x -> ([x] ++)) <$> headerPacket <*> dataPackets)
    packetList = Gen.int (Range.linear 0 100) >>= (\x -> concat <$> replicateM x packet)

    model = depacketizerModel @dataWidth @20 @EthernetHeader @IPv4HeaderLite @IPv4Header (const . toLite)

-- | Tests the IP depacketizer for arbitrary packets
testIPPacketizer
  :: forall (dataWidth :: C.Nat) k
   . ( C.KnownNat dataWidth
     , 1 C.<= dataWidth
     , dataWidth ~ 2 C.* k
     )
  => C.SNat dataWidth
  -> Property
testIPPacketizer _ = idWithModelSingleDomain
  @C.System defExpectOptions
  (fmap fullPackets (Gen.list (Range.linear 1 100) (genPackets (C.unpack <$> Gen.enumBounded))))
  (C.exposeClockResetEnable model)
  (C.exposeClockResetEnable (ipDepacketizerC @C.System @dataWidth))
  where
    model ps =
     let
       ps' = depacketizerModel const ps
       ps'' = fmap toLite <$> ps'
       invalidChecksums = (\h -> 0xFFFF /= pureInternetChecksum (C.bitCoerce h :: C.Vec 10 (C.BitVector 16))) . _meta <$> ps'
       invalidIHLs = (/= 5) . _ipv4Ihl . _meta <$> ps'
       aborts = zipWith (||) invalidChecksums invalidIHLs
      in
       concat $ zipWith (\qs abort -> (\q -> q {_abort = _abort q || abort}) <$> qs) (chunkByPacket ps'') (aborts ++ repeat False)

prop_ip_depacketizer_valid_d2 :: Property
prop_ip_depacketizer_valid_d2 = testIPPacketizer C.d2

prop_ip_depacketizer_valid_d6 :: Property
prop_ip_depacketizer_valid_d6 = testIPPacketizer C.d6

prop_ip_depacketizer_valid_d18 :: Property
prop_ip_depacketizer_valid_d18 = testIPPacketizer C.d18

prop_ip_depacketizer_valid_d20 :: Property
prop_ip_depacketizer_valid_d20 = testIPPacketizer C.d20

prop_ip_depacketizer_valid_d28 :: Property
prop_ip_depacketizer_valid_d28 = testIPPacketizer C.d28

prop_ip_depacketizer_d2 :: Property
prop_ip_depacketizer_d2 = testIPPacketizer C.d2

prop_ip_depacketizer_d6 :: Property
prop_ip_depacketizer_d6 = testIPPacketizer C.d6

prop_ip_depacketizer_d18 :: Property
prop_ip_depacketizer_d18 = testIPPacketizer C.d18

prop_ip_depacketizer_d20 :: Property
prop_ip_depacketizer_d20 = testIPPacketizer C.d20

prop_ip_depacketizer_d28 :: Property
prop_ip_depacketizer_d28 = testIPPacketizer C.d28

tests :: TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1_000))
  $(testGroupGenerator)
