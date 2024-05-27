{-# language FlexibleContexts #-}
{-# language NumericUnderscores #-}
{-# language RecordWildCards #-}

module Test.Cores.Ethernet.Mac.Preamble where

-- base
import Data.List qualified as L
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
import Protocols.Extra.PacketStream
import Protocols.Hedgehog

-- ethernet
import Clash.Cores.Ethernet.Mac.EthernetTypes
import Clash.Cores.Ethernet.Mac.Preamble ( preambleInserterC, preambleStripperC )

-- tests
import Test.Protocols.Extra.PacketStream.Extra
import Test.Protocols.Extra.PacketStream.Packetizers ( depacketizerModel, packetizerModel )


genVec :: (C.KnownNat n, 1 <= n) => Gen a -> Gen (C.Vec n a)
genVec gen = sequence (C.repeat gen)

preambleInserterPropertyGenerator
  :: forall (dataWidth :: Nat) .
     1 <= dataWidth
  => SNat dataWidth
  -> Property
preambleInserterPropertyGenerator SNat =
  propWithModelSingleDomain
    @C.System
    defExpectOptions
    (fmap fullPackets (Gen.list (Range.linear 1 100) genPackets))
    (C.exposeClockResetEnable model)
    (C.exposeClockResetEnable @C.System preambleInserterC)
    (===)
    where
      model :: [PacketStreamM2S dataWidth ()] -> [PacketStreamM2S dataWidth ()]
      model = packetizerModel (const ()) id . L.map (\x -> x { _meta = preamble })
      genPackets =
          PacketStreamM2S <$>
          genVec Gen.enumBounded <*>
          Gen.maybe Gen.enumBounded <*>
          Gen.enumBounded <*>
          Gen.enumBounded

-- | n mod dataWidth ~ 1
prop_preamble_inserter_d1 :: Property
prop_preamble_inserter_d1 = preambleInserterPropertyGenerator d1

-- | n mod dataWidth ~ 3
prop_preamble_inserter_d3 :: Property
prop_preamble_inserter_d3 = preambleInserterPropertyGenerator d3

-- | n mod dataWidth ~ 0
prop_preamble_inserter_d7 :: Property
prop_preamble_inserter_d7 = preambleInserterPropertyGenerator d7

-- | dataWidth < header byte size
prop_preamble_inserter_d9 :: Property
prop_preamble_inserter_d9 = preambleInserterPropertyGenerator d9

-- | dataWidth ~ header byte size
prop_preamble_inserter_d14 :: Property
prop_preamble_inserter_d14 = preambleInserterPropertyGenerator d14

-- | dataWidth > header byte size
prop_preamble_inserter_d15 :: Property
prop_preamble_inserter_d15 = preambleInserterPropertyGenerator d15

preambleStripperPropertyGenerator
  :: forall (dataWidth :: Nat) .
     1 <= dataWidth
  => SNat dataWidth
  -> Property
preambleStripperPropertyGenerator SNat =
  propWithModelSingleDomain
    @C.System
    defExpectOptions
    (fmap fullPackets (Gen.list (Range.linear 1 100) genPackets))
    (C.exposeClockResetEnable model)
    (C.exposeClockResetEnable @C.System preambleStripperC)
    (===)
    where
      model ps = validateAll (depacketizerModel const ps)

      validateAll :: [PacketStreamM2S dataWidth Preamble] -> [PacketStreamM2S dataWidth ()]
      validateAll ps = L.concatMap validatePreamble (chunkByPacket ps)

      validatePreamble :: [PacketStreamM2S dataWidth Preamble] -> [PacketStreamM2S dataWidth ()]
      validatePreamble ps = if C.last (_meta $ Prelude.head ps) == startFrameDelimiter
                            then L.map (\p -> p {_meta = ()}) ps
                            else []
      genPackets =
          PacketStreamM2S <$>
          genVec Gen.enumBounded <*>
          Gen.maybe Gen.enumBounded <*>
          Gen.enumBounded <*>
          Gen.enumBounded

-- | n mod dataWidth ~ 1
prop_preamble_stripper_d1 :: Property
prop_preamble_stripper_d1 = preambleStripperPropertyGenerator d1

-- | n mod dataWidth ~ 3
prop_preamble_stripper_d5 :: Property
prop_preamble_stripper_d5 = preambleStripperPropertyGenerator d5

-- | n mod dataWidth ~ 0
prop_preamble_stripper_d4 :: Property
prop_preamble_stripper_d4 = preambleStripperPropertyGenerator d4

-- | dataWidth < header byte size
prop_preamble_stripper_d7 :: Property
prop_preamble_stripper_d7 = preambleStripperPropertyGenerator d7

-- | dataWidth ~ header byte size
prop_preamble_stripper_d8 :: Property
prop_preamble_stripper_d8 = preambleStripperPropertyGenerator d8

-- | dataWidth > header byte size
prop_preamble_stripper_d9 :: Property
prop_preamble_stripper_d9 = preambleStripperPropertyGenerator d9

tests :: TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1_000))
  $(testGroupGenerator)
