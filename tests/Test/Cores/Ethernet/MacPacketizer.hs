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
--import Clash.Cores.Ethernet.MacPacketizer ( EthernetHeader, macDepacketizerC )
import Clash.Cores.Ethernet.PacketStream
import Clash.Cores.Ethernet.EthernetTypes

import Test.Cores.Ethernet.Packetizer ( packetizerModel )
import Test.Cores.Ethernet.Util

genVec :: (C.KnownNat n, 1 <= n) => Gen a -> Gen (C.Vec n a)
genVec gen = sequence (C.repeat gen)

macPacketizerPropertyGenerator
  :: forall (dataWidth :: Nat).
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
      macPacketizerC = errorX "not implemented error" -- TODO create circuit
      model :: [PacketStreamM2S dataWidth EthernetHeader] -> [PacketStreamM2S dataWidth ()]
      model = packetizerModel (const ()) const
      genPackets =
          PacketStreamM2S <$>
          genVec Gen.enumBounded <*>
          Gen.maybe Gen.enumBounded <*>
          Gen.enumBounded <*>
          Gen.enumBounded

-- | n mod dataWidth ~ 1
prop_mac_packetizer_d1 :: Property
prop_mac_packetizer_d1 = macPacketizerPropertyGenerator d1

-- | n mod dataWidth ~ 3
prop_mac_packetizer_d3 :: Property
prop_mac_packetizer_d3 = macPacketizerPropertyGenerator d3

-- | n mod dataWidth ~ 1
prop_mac_packetizer_d7 :: Property
prop_mac_packetizer_d7 = macPacketizerPropertyGenerator d3

-- | dataWidth < header byte size
prop_mac_packetizer_d9 :: Property
prop_mac_packetizer_d9 = macPacketizerPropertyGenerator d9

-- | dataWidth ~ header byte size
prop_mac_packetizer_d14 :: Property
prop_mac_packetizer_d14 = macPacketizerPropertyGenerator d14

-- | dataWidth > header byte size
prop_mac_packetizer_d15 :: Property
prop_mac_packetizer_d15 = macPacketizerPropertyGenerator d15

tests :: TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1_000))
  $(testGroupGenerator)
