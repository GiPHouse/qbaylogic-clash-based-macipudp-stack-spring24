{-# language FlexibleContexts #-}
{-# language NumericUnderscores #-}
{-# language RecordWildCards #-}

module Test.Cores.Ethernet.IpPacketizer where

-- base
import Control.Monad
import Prelude

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
import Clash.Cores.Ethernet.IpPacketizer
import Clash.Cores.Ethernet.PacketStream

import Test.Cores.Ethernet.Packetizer ( packetizerModel )
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
    genIpAddr = C.sequence (C.repeat @4 geb)
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


tests :: TestTree
tests =
    localOption (mkTimeout 20_000_000 {- 20 seconds -})
  $ localOption (HedgehogTestLimit (Just 1_000))
  $(testGroupGenerator)
