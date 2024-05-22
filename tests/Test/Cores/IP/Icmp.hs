{-# language FlexibleContexts #-}
{-# language NumericUnderscores #-}
{-# language RecordWildCards #-}

module Test.Cores.IP.Icmp where

-- base
import Prelude

-- clash-prelude
import Clash.Prelude hiding ( concatMap )
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
import Clash.Cores.Ethernet.PacketStream
import Clash.Cores.IP.Icmp
import Clash.Cores.IP.IcmpTypes
import Clash.Cores.IP.IPv4Types

import Test.Cores.Ethernet.Depacketizer ( depacketizerModel )
import Test.Cores.Ethernet.Util

genVec :: (C.KnownNat n, 1 <= n) => Gen a -> Gen (C.Vec n a)
genVec gen = sequence (C.repeat gen)

icmpReceiverPropertyGenerator
  :: forall (dataWidth :: Nat).
  1 <= dataWidth
  => SNat dataWidth
  -> Property
icmpReceiverPropertyGenerator C.SNat =
  propWithModelSingleDomain
    @C.System
    defExpectOptions
    (fmap fullPackets (Gen.list (Range.linear 1 100) (genPackets genIPv4HeaderLite)))
    (C.exposeClockResetEnable model)
    (C.exposeClockResetEnable @C.System icmpReceiverC)
    (===)
    where
      f :: IcmpHeader -> IPv4HeaderLite -> (IPv4HeaderLite, IcmpHeaderLite)
      f IcmpHeader{..} ipheader = (ipheader,  IcmpHeaderLite{_typeL = _type, _checksumL = _checksum})

      model :: [PacketStreamM2S dataWidth IPv4HeaderLite] -> [PacketStreamM2S dataWidth (IPv4HeaderLite, IcmpHeaderLite)]
      model = depacketizerModel f

      genPackets :: Gen IPv4HeaderLite -> Gen (PacketStreamM2S dataWidth IPv4HeaderLite)
      genPackets genMeta =
        PacketStreamM2S <$>
        genVec Gen.enumBounded <*>
        Gen.maybe Gen.enumBounded <*>
        genMeta <*>
        Gen.enumBounded

      genIpAddr :: Gen IPv4Address
      genIpAddr = IPv4Address <$> C.sequence (C.repeat @4 Gen.enumBounded)

      genIPv4HeaderLite :: Gen IPv4HeaderLite
      genIPv4HeaderLite = IPv4HeaderLite <$> genIpAddr <*> genIpAddr <*> Gen.enumBounded

prop_icmp_receiver_d1 :: Property
prop_icmp_receiver_d1 = icmpReceiverPropertyGenerator d1

prop_icmp_receiver_d4 :: Property
prop_icmp_receiver_d4 = icmpReceiverPropertyGenerator d4

prop_icmp_receiver_d16 :: Property
prop_icmp_receiver_d16 = icmpReceiverPropertyGenerator d16

prop_icmp_receiver_d23 :: Property
prop_icmp_receiver_d23 = icmpReceiverPropertyGenerator d23

prop_icmp_receiver_d24 :: Property
prop_icmp_receiver_d24 = icmpReceiverPropertyGenerator d24

prop_icmp_receiver_d25 :: Property
prop_icmp_receiver_d25 = icmpReceiverPropertyGenerator d25


tests :: TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1_000))
  $(testGroupGenerator)
