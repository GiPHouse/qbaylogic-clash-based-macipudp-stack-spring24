{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Cores.Ethernet.PacketStream where

-- base
import Prelude

-- clash-prelude
import qualified Clash.Prelude as C
import Clash.Prelude (type (<=))

-- hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- tasty
import Test.Tasty
import Test.Tasty.Hedgehog (HedgehogTestLimit(HedgehogTestLimit))
import Test.Tasty.Hedgehog.Extra (testProperty)
import Test.Tasty.TH (testGroupGenerator)

-- clash-protocols
import Protocols
import Protocols.Hedgehog

-- Me
import Clash.Cores.Ethernet.PacketStream

genVec :: (C.KnownNat n, 1 <= n) => Gen a -> Gen (C.Vec n a)
genVec gen = sequence (C.repeat gen)

-- | Test the packet stream instance
--   TODO: Use the fifo given by `DfConv`
prop_packetstream_sometest_id :: Property
prop_packetstream_sometest_id =
  propWithModelSingleDomain
    @C.System
    defExpectOptions
    (Gen.list (Range.linear 0 100) genPackets)
    (C.exposeClockResetEnable $ error "The model of the circuit: Implement a function here that transform the inputs to the circuit to outputs")
    (C.exposeClockResetEnable @C.System ckt)
    (\_a _b -> error "Property to test for. Function is given the data produced by the model as a first argument, and the sampled data as a second argument.")
 where
  ckt :: (C.HiddenClockResetEnable dom) =>
    Circuit
      (PacketStream dom 1 Int)
      (PacketStream dom 1 Int)
  ckt = error "Insert the circuit to test here"

  -- This is used to generate
  genPackets =
    PacketStreamM2S <$>
    (genVec Gen.enumBounded) <*>
    (Gen.maybe Gen.enumBounded) <*>
    Gen.enumBounded <*>
    Gen.enumBounded

tests :: TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1000))
  $(testGroupGenerator)
