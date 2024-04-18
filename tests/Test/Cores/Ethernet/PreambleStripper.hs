{-# language FlexibleContexts #-}
{-# language NumericUnderscores #-}
{-# language RecordWildCards #-}

module Test.Cores.Ethernet.PreambleStripper where

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
import Clash.Cores.Ethernet.PreambleStripper
import Clash.Cores.Ethernet.PacketStream

import Test.Cores.Ethernet.Depacketizer ( depacketizerModel )
import Test.Cores.Ethernet.Util

import Data.List qualified as L


genVec :: (C.KnownNat n, 1 <= n) => Gen a -> Gen (C.Vec n a)
genVec gen = sequence (C.repeat gen)

preambleStripperPropertyGenerator
  :: forall (dataWidth :: Nat).
  ( KnownNat dataWidth
  , 1 <= dataWidth
  )
  => SNat dataWidth
  -> Property
preambleStripperPropertyGenerator _ =
  propWithModelSingleDomain
    @C.System
    defExpectOptions
    (fmap fullPackets (Gen.list (Range.linear 1 100) genPackets))
    (C.exposeClockResetEnable model)
    (C.exposeClockResetEnable @C.System preambleStripperC)
    (===)
    where
      model ps = model2 (model1 ps)
      model1 :: [PacketStreamM2S dataWidth ()] -> [PacketStreamM2S dataWidth Preamble]
      model1 = depacketizerModel const
      model2 :: [PacketStreamM2S dataWidth Preamble] -> [PacketStreamM2S dataWidth ()]
      model2 ps = L.concatMap validatePreamble (chunkByPacket ps)

      startFrameDelimiter :: BitVector 8
      startFrameDelimiter = 0xD5

      validatePreamble :: [PacketStreamM2S dataWidth Preamble] -> [PacketStreamM2S dataWidth ()]
      validatePreamble ps = if (C.last $ _meta $ Prelude.head ps) == startFrameDelimiter
                            then L.map (\p -> p {_meta = ()}) ps
                            else []--L.map (\p -> p {_meta = ()}) ps

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
prop_preamble_stripper_d3 :: Property
prop_preamble_stripper_d3 = preambleStripperPropertyGenerator d3

-- | n mod dataWidth ~ 1
prop_preamble_stripper_d7 :: Property
prop_preamble_stripper_d7 = preambleStripperPropertyGenerator d3

-- | dataWidth < header byte size
prop_preamble_stripper_d9 :: Property
prop_preamble_stripper_d9 = preambleStripperPropertyGenerator d9

-- | dataWidth ~ header byte size
prop_preamble_stripper_d14 :: Property
prop_preamble_stripper_d14 = preambleStripperPropertyGenerator d14

-- | dataWidth > header byte size
prop_preamble_stripper_d15 :: Property
prop_preamble_stripper_d15 = preambleStripperPropertyGenerator d15

tests :: TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1_000))
  $(testGroupGenerator)
