{-# language FlexibleContexts #-}
{-# language NumericUnderscores #-}
{-# language RecordWildCards #-}

module Test.Cores.Ethernet.PreambleInserter where

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
import Clash.Cores.Ethernet.Mac.EthernetTypes
import Clash.Cores.Ethernet.Mac.Preamble ( preambleInserterC )
import Protocols.Extra.PacketStream

import Test.Cores.Ethernet.Packetizer ( packetizerModel )
import Test.Cores.Ethernet.Util

import Data.List qualified as L


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

tests :: TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1_000))
  $(testGroupGenerator)
