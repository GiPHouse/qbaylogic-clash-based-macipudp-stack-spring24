{-# language FlexibleContexts #-}
{-# language NumericUnderscores #-}
{-# language RecordWildCards #-}

module Test.Cores.Ethernet.PacketStream where

-- base
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

genVec :: (C.KnownNat n, 1 <= n) => Gen a -> Gen (C.Vec n a)
genVec gen = sequence (C.repeat gen)

-- | Test the packet stream instance
--   TODO: Use the fifo given by `DfConv`
prop_packetstream_fifo_id :: Property
prop_packetstream_fifo_id =
  propWithModelSingleDomain
    @C.System
    defExpectOptions
    (Gen.list (Range.linear 0 100) genPackets)
    (C.exposeClockResetEnable id)
    (C.exposeClockResetEnable @C.System ckt)
    (\a b -> a === b)
 where
  ckt :: (C.HiddenClockResetEnable dom) =>
    Circuit
      (PacketStream dom 1 Int)
      (PacketStream dom 1 Int)
  ckt = DfConv.fifo Proxy Proxy (C.SNat @10)

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
