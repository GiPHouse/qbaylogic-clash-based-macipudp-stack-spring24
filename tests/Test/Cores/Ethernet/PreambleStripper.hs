{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Cores.Ethernet.PreambleStripper where

-- base
import Prelude

-- clash-prelude
import Clash.Prelude hiding (concatMap)
import Clash.Prelude qualified as C

-- hedgehog
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

-- tasty
import Test.Tasty
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))
import Test.Tasty.Hedgehog.Extra (testProperty)
import Test.Tasty.TH (testGroupGenerator)

-- clash-protocols
import Protocols.Hedgehog

-- Me
import Clash.Cores.Ethernet.EthernetTypes
import Clash.Cores.Ethernet.PacketStream
import Clash.Cores.Ethernet.PreambleStripper

import Test.Cores.Ethernet.Depacketizer (depacketizerModel)
import Test.Cores.Ethernet.Util

import Data.List qualified as L

genVec :: (C.KnownNat n, 1 <= n) => Gen a -> Gen (C.Vec n a)
genVec gen = sequence (C.repeat gen)

preambleStripperPropertyGenerator
  :: forall (dataWidth :: Nat)
   . 1 <= dataWidth
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
  validatePreamble ps =
    if C.last (_meta $ Prelude.head ps) == startFrameDelimiter
      then L.map (\p -> p{_meta = ()}) ps
      else []
  genPackets =
    PacketStreamM2S
      <$> genVec Gen.enumBounded
      <*> Gen.maybe Gen.enumBounded
      <*> Gen.enumBounded
      <*> Gen.enumBounded

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
  localOption (mkTimeout 12_000_000 {- 12 seconds -}) $
    localOption
      (HedgehogTestLimit (Just 1_000))
      $(testGroupGenerator)
