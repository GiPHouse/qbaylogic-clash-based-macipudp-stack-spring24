{-# language FlexibleContexts #-}
{-# language NumericUnderscores #-}
{-# language RecordWildCards #-}

module Test.Cores.Ethernet.DownConverter where

-- base
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
import Protocols.Hedgehog

-- util module
import Test.Cores.Ethernet.Util

-- ethernet modules
import Protocols.Extra.PacketStream
import Protocols.Extra.PacketStream.Converters

genVec :: (C.KnownNat n, 1 <= n) => Gen a -> Gen (C.Vec n a)
genVec gen = sequence (C.repeat gen)

model :: forall n. 1 <= n => C.KnownNat n => [PacketStreamM2S n ()] -> [PacketStreamM2S 1 ()]
model fragments = fragments >>= chopPacket

-- | Test the downconverter stream instance
downconverterTest :: forall n. 1 <= n => C.SNat n -> Property
downconverterTest C.SNat =
  propWithModelSingleDomain
    @C.System
    defExpectOptions
    (Gen.list (Range.linear 0 100) genPackets)                  -- Input packets
    (C.exposeClockResetEnable model)                            -- Desired behaviour of DownConverter
    (C.exposeClockResetEnable @C.System (ckt @n))               -- Implementation of DownConverter
    (===)                                                       -- Property to test
  where
    ckt :: forall (dataWidth :: C.Nat) (dom :: C.Domain).
      C.HiddenClockResetEnable dom
      => 1 <= dataWidth
      => C.KnownNat dataWidth
      => Circuit (PacketStream dom dataWidth ()) (PacketStream dom 1 ())
    ckt = downConverterC

    -- This generates the packets
    genPackets =
      PacketStreamM2S <$>
      genVec Gen.enumBounded <*>
      Gen.maybe Gen.enumBounded <*>
      Gen.enumBounded <*>
      Gen.enumBounded

prop_downconverter_d1, prop_downconverter_d2, prop_downconverter_d4 :: Property
prop_downconverter_d1 = downconverterTest (C.SNat @1)
prop_downconverter_d2 = downconverterTest (C.SNat @2)
prop_downconverter_d4 = downconverterTest (C.SNat @4)

tests :: TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1_000))
  $(testGroupGenerator)
