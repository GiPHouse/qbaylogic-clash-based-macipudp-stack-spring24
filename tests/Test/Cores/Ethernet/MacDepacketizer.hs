{-# language FlexibleContexts #-}
{-# language NumericUnderscores #-}
{-# language RecordWildCards #-}

module Test.Cores.Ethernet.MacDepacketizer where

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
import Clash.Cores.Ethernet.MacDepacketizer ( EthernetHeader, macDepacketizerC )
import Clash.Cores.Ethernet.PacketStream

import Test.Cores.Ethernet.Depacketizer ( depacketizerModel )
import Test.Cores.Ethernet.Util


genVec :: (C.KnownNat n, 1 <= n) => Gen a -> Gen (C.Vec n a)
genVec gen = sequence (C.repeat gen)

macDepacketizerPropertyGenerator :: forall (dataWidth :: Nat) (m :: Nat) .
  ( KnownNat dataWidth
  , 1 <= dataWidth
  , Mod 14 dataWidth + m ~ dataWidth)
  => SNat dataWidth
  -> Property
macDepacketizerPropertyGenerator dataWidth =
  propWithModelSingleDomain
    @C.System
    defExpectOptions
    (fmap addPacketWithLastSet (Gen.list (Range.linear 1 100) genPackets))
    (C.exposeClockResetEnable model)
    (C.exposeClockResetEnable @C.System (macDepacketizerC dataWidth))
    (===)
    where
      model = depacketizerModel dataWidth d14 :: [PacketStreamM2S dataWidth ()] -> [PacketStreamM2S dataWidth EthernetHeader]
      genPackets =
          PacketStreamM2S <$>
          genVec Gen.enumBounded <*>
          Gen.maybe Gen.enumBounded <*>
          Gen.enumBounded <*>
          Gen.enum False False

-- | n mod dataWidth ~ 0
prop_mac_depacketizer_d7 :: Property
prop_mac_depacketizer_d7 = macDepacketizerPropertyGenerator d7

-- | dataWidth < header byte size
prop_mac_depacketizer_d9 :: Property
prop_mac_depacketizer_d9 = macDepacketizerPropertyGenerator d9

-- | dataWidth ~ header byte size
prop_mac_depacketizer_d14 :: Property
prop_mac_depacketizer_d14 = macDepacketizerPropertyGenerator d14

-- | dataWidth > header byte size
prop_mac_depacketizer_d15 :: Property
prop_mac_depacketizer_d15 = macDepacketizerPropertyGenerator d15

tests :: TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1_000))
  $(testGroupGenerator)
