{-# language FlexibleContexts #-}
{-# language NumericUnderscores #-}
{-# language RecordWildCards #-}

module Test.Cores.Ethernet.Mac.MacPacketizers where

import Prelude

import Clash.Prelude hiding ( concatMap )
import Clash.Prelude qualified as C

import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Test.Tasty
import Test.Tasty.Hedgehog ( HedgehogTestLimit(HedgehogTestLimit) )
import Test.Tasty.Hedgehog.Extra ( testProperty )
import Test.Tasty.TH ( testGroupGenerator )

import Protocols.Extra.PacketStream
import Protocols.Hedgehog
import Test.Protocols.Extra.PacketStream.Depacketizer ( depacketizerModel )
import Test.Protocols.Extra.PacketStream.Packetizer ( packetizerModel )

import Clash.Cores.Ethernet.Mac.EthernetTypes
import Clash.Cores.Ethernet.Mac.MacPacketizers ( macDepacketizerC, macPacketizerC )

import Test.Cores.Ethernet.Util


genVec :: (C.KnownNat n, 1 <= n) => Gen a -> Gen (C.Vec n a)
genVec gen = sequence (C.repeat gen)

genMeta :: forall (meta :: Type) (metaBytes :: Nat)
   . KnownNat metaBytes
  => 1 <= metaBytes
  => BitPack meta
  => BitSize meta ~ metaBytes * 8
  => Gen meta
genMeta = fmap bitCoerce (genVec Gen.enumBounded :: Gen (Vec metaBytes (BitVector 8)))

macPacketizerPropertyGenerator
  :: forall (dataWidth :: Nat) .
  ( KnownNat dataWidth
  , 1 <= dataWidth)
  => SNat dataWidth
  -> Property
macPacketizerPropertyGenerator _ =
  propWithModelSingleDomain
    @C.System
    defExpectOptions
    (fmap fullPackets (Gen.list (Range.linear 1 100) genPackets))
    (C.exposeClockResetEnable model)
    (C.exposeClockResetEnable @C.System macPacketizerC)
    (===)
    where
      model :: [PacketStreamM2S dataWidth EthernetHeader] -> [PacketStreamM2S dataWidth ()]
      model = packetizerModel (const ()) id
      genPackets =
          PacketStreamM2S <$>
          genVec Gen.enumBounded <*>
          Gen.maybe Gen.enumBounded <*>
          genMeta <*>
          Gen.enumBounded

-- | n mod dataWidth ~ 1
prop_mac_packetizer_d1 :: Property
prop_mac_packetizer_d1 = macPacketizerPropertyGenerator d1

-- | n mod dataWidth ~ 3
prop_mac_packetizer_d3 :: Property
prop_mac_packetizer_d3 = macPacketizerPropertyGenerator d3

-- | n mod dataWidth ~ 0
prop_mac_packetizer_d7 :: Property
prop_mac_packetizer_d7 = macPacketizerPropertyGenerator d7

-- | dataWidth < header byte size
prop_mac_packetizer_d9 :: Property
prop_mac_packetizer_d9 = macPacketizerPropertyGenerator d9

-- | dataWidth ~ header byte size
prop_mac_packetizer_d14 :: Property
prop_mac_packetizer_d14 = macPacketizerPropertyGenerator d14

-- | dataWidth > header byte size
prop_mac_packetizer_d15 :: Property
prop_mac_packetizer_d15 = macPacketizerPropertyGenerator d15

macDepacketizerPropertyGenerator
  :: forall (dataWidth :: Nat)
   . 1 <= dataWidth
  => SNat dataWidth
  -> Property
macDepacketizerPropertyGenerator SNat =
  propWithModelSingleDomain
    @C.System
    defExpectOptions
    (fmap fullPackets (Gen.list (Range.linear 1 100) genPackets))
    (C.exposeClockResetEnable model)
    (C.exposeClockResetEnable @C.System macDepacketizerC)
    (===)
    where
      model :: [PacketStreamM2S dataWidth ()] -> [PacketStreamM2S dataWidth EthernetHeader]
      model = depacketizerModel const
      genPackets =
          PacketStreamM2S <$>
          genVec Gen.enumBounded <*>
          Gen.maybe Gen.enumBounded <*>
          Gen.enumBounded <*>
          Gen.enumBounded

-- | n mod dataWidth ~ 1
prop_mac_depacketizer_d1 :: Property
prop_mac_depacketizer_d1 = macDepacketizerPropertyGenerator d1

-- | n mod dataWidth ~ 3
prop_mac_depacketizer_d3 :: Property
prop_mac_depacketizer_d3 = macDepacketizerPropertyGenerator d3

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
