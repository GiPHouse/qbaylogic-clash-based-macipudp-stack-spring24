{-# language FlexibleContexts #-}
{-# language NumericUnderscores #-}
{-# language RecordWildCards #-}

module Test.Cores.Ethernet.InternetChecksum where

-- base
import Data.Maybe
import Data.Proxy
import Prelude

-- clash-prelude
import Clash.Prelude ( type (<=) )
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
import Protocols
import Protocols.DfConv qualified as DfConv
import Protocols.Hedgehog

-- Me
import Clash.Cores.Ethernet.PacketStream
import Test.Cores.Ethernet.MaybeControl ( propWithModelMaybeControlSingleDomain )

genVec :: (C.KnownNat n, 1 <= n) => Gen a -> Gen (C.Vec n a)
genVec gen = sequence (C.repeat gen)

lastTrue :: [(PacketStreamM2S 4 (), Bool)] -> [(PacketStreamM2S 4 (), Bool)]
lastTrue [] = []
lastTrue fragments = init fragments ++ [(fst . last $ fragments, True)]

genWord :: Gen (PacketStreamM2S 4 ())
genWord = PacketStreamM2S <$>
          genVec Gen.enumBounded <*>
          Gen.maybe Gen.enumBounded <*>
          Gen.enumBounded <*>
          Gen.enumBounded

genTuple :: Gen (PacketStreamM2S 4 (), Bool)
genTuple = (,) <$> genWord <*> Gen.bool

genInputList :: Range Int -> Gen [(PacketStreamM2S 4 (), Bool)]
genInputList range = Gen.list range genInput
  where
    genInput = (,) <$> genWord <*> pure False


prop_checksum :: Property
prop_checksum =
  property $ do
    -- input <- forAll $ genInputList (Range.linear 0 100)

    -- result <- runSim $ do
    --   let input' = C.fromList input internetChecksum input

    success


tests :: TestTree
tests =
    localOption (mkTimeout 10_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1_000))
  $(testGroupGenerator)
