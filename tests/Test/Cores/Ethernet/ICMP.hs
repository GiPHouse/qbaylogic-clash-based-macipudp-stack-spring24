{-# language FlexibleContexts #-}
{-# language NumericUnderscores #-}
{-# language RecordWildCards #-}

module Test.Cores.Ethernet.ICMP where

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
import Clash.Cores.Ethernet.EthernetTypes
import Clash.Cores.Ethernet.ICMP
import Clash.Cores.Ethernet.PacketStream

import Test.Cores.Ethernet.Depacketizer ( depacketizerModel )
import Test.Cores.Ethernet.Util

genVec :: (C.KnownNat n, 1 <= n) => Gen a -> Gen (C.Vec n a)
genVec gen = sequence (C.repeat gen)

icmpReceiverPropertyGenerator
  :: forall (dataWidth :: Nat).
  ( KnownNat dataWidth
  , 1 <= dataWidth
  )
  => SNat dataWidth
  -> Property
icmpReceiverPropertyGenerator _ =
  propWithModelSingleDomain
    @C.System
    defExpectOptions
    (fmap fullPackets (Gen.list (Range.linear 1 100) (genPackets genIPv4HeaderLite)))
    (C.exposeClockResetEnable model)
    (C.exposeClockResetEnable @C.System icmpReceiverC)
    (===)
    where
      f :: IcmpHeader -> IPv4HeaderLite -> (IPv4HeaderLite, IcmpHeaderLite)
      f IcmpHeader{..} ipheader = (ipheader,  IcmpHeaderLite{_typeL = _type})

      model :: [PacketStreamM2S dataWidth IPv4HeaderLite] -> [PacketStreamM2S dataWidth (IPv4HeaderLite, IcmpHeaderLite)]
      model = depacketizerModel f

      genPackets :: Gen IPv4HeaderLite -> Gen (PacketStreamM2S dataWidth IPv4HeaderLite)
      genPackets genMeta =
        PacketStreamM2S <$>
        genVec Gen.enumBounded <*>
        Gen.maybe Gen.enumBounded <*>
        genMeta <*>
        Gen.enumBounded

      genIpAddr = C.sequence (C.repeat @4 Gen.enumBounded)
      genIPv4HeaderLite = IPv4HeaderLite <$> genIpAddr <*> genIpAddr <*> Gen.enumBounded



-- | n mod dataWidth ~ 1
prop_icmp_receiver_d1 :: Property
prop_icmp_receiver_d1 = icmpReceiverPropertyGenerator d1


tests :: TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1_000))
  $(testGroupGenerator)
