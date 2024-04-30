{-# language FlexibleContexts #-}
{-# language NumericUnderscores #-}
{-# language RecordWildCards #-}

module Test.Cores.Ethernet.MacPacketizer where

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
import Clash.Cores.Ethernet.MacPacketizer
import Clash.Cores.Ethernet.PacketStream

import Test.Cores.Ethernet.Packetizer ( packetizerModel )
import Test.Cores.Ethernet.Util

genVec :: (C.KnownNat n, 1 <= n) => Gen a -> Gen (C.Vec n a)
genVec gen = sequence (C.repeat gen)

genMeta :: forall (meta :: Type) (metaBytes :: Nat)
   . KnownNat metaBytes
  => 1 <= metaBytes
  => BitPack meta
  => BitSize meta ~ metaBytes * 8
  => Gen meta
genMeta = fmap (unpack . pack) (genVec Gen.enumBounded :: Gen (Vec metaBytes (BitVector 8)))

macPacketizerPropertyGenerator
  :: forall (dataWidth :: Nat).
  ( KnownNat dataWidth
  , 1 <= dataWidth
  , Mod 14 dataWidth <= (dataWidth - 1))
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
          Gen.enum False False--Gen.enumBounded

-- | n mod dataWidth ~ 1
--prop_mac_packetizer_d1 :: Property
--prop_mac_packetizer_d1 = macPacketizerPropertyGenerator d1

-- | n mod dataWidth ~ 3
prop_mac_packetizer_d3 :: Property
prop_mac_packetizer_d3 = macPacketizerPropertyGenerator d3

-- | n mod dataWidth ~ 1
--prop_mac_packetizer_d7 :: Property
--prop_mac_packetizer_d7 = macPacketizerPropertyGenerator d3

-- | dataWidth < header byte size
--prop_mac_packetizer_d9 :: Property
--prop_mac_packetizer_d9 = macPacketizerPropertyGenerator d9

-- | dataWidth ~ header byte size
--prop_mac_packetizer_d14 :: Property
--prop_mac_packetizer_d14 = macPacketizerPropertyGenerator d14

-- | dataWidth > header byte size
--prop_mac_packetizer_d15 :: Property
--prop_mac_packetizer_d15 = macPacketizerPropertyGenerator d15-}

tests :: TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1_000))
  $(testGroupGenerator)
