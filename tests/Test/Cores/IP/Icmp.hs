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

      testAddress :: Gen IPv4Address
      testAddress = pure $ IPv4Address (C.repeat 0x00)

      genIPv4HeaderLite :: Gen IPv4HeaderLite
      genIPv4HeaderLite = IPv4HeaderLite <$> testAddress <*> testAddress <*> pure 0

prop_icmp_receiver_d1 :: Property
prop_icmp_receiver_d1 = icmpReceiverPropertyGenerator d1

prop_icmp_receiver_d2 :: Property
prop_icmp_receiver_d2 = icmpReceiverPropertyGenerator d2

prop_icmp_receiver_d3 :: Property
prop_icmp_receiver_d3 = icmpReceiverPropertyGenerator d3

prop_icmp_receiver_d4 :: Property
prop_icmp_receiver_d4 = icmpReceiverPropertyGenerator d4

tests :: TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1_000))
  $(testGroupGenerator)
