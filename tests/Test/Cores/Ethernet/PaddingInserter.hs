{-# language FlexibleContexts #-}
{-# language NumericUnderscores #-}
{-# language RecordWildCards #-}

module Test.Cores.Ethernet.PaddingInserter where

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
import Clash.Cores.Ethernet.PacketStream
import Clash.Cores.Ethernet.PaddingInserter

import Data.Maybe

genVec :: (C.KnownNat n, 1 <= n) => Gen a -> Gen (C.Vec n a)
genVec gen = sequence (C.repeat gen)

model
  :: forall (dataWidth :: C.Nat).
  1 <= dataWidth
  => C.KnownNat dataWidth
  => [PacketStreamM2S dataWidth ()]
  -> [PacketStreamM2S dataWidth ()]
model fragments = concatMap (setLast . removeLasts . paddingInserter) (chunkByPacket fragments)
  where
    -- Calculate ceil(64/dataWidth) packets, which is the required
    -- amount to fulfill the minimum ethernet frame size of 64
    neededPadding pkt = max 0 (div (63 + (C.natToNum @dataWidth)) (C.natToNum @dataWidth) - length pkt)
    padding = PacketStreamM2S {_data = C.repeat 0, _last = Nothing, _meta = (), _abort = False}
    paddingInserter pkt = pkt ++ replicate (neededPadding pkt) padding

    --Set the _last of every stream to Nothing except for the last stream
    removeLasts pkt = map (\x -> x{_last = Nothing}) (init pkt) ++ [last pkt]

    -- Calculate the index of _last needed after padding
    calcMod = mod (63 :: C.Index (C.Max 64 (dataWidth C.+ 1))) (C.natToNum @dataWidth)
    -- Calculate the actual index of _last of the last stream
    calcLast pkt = max (fromMaybe (0 :: C.Index dataWidth) (_last (last pkt))) (C.resize calcMod)
    setLast pkt
      -- If no padding was added, then we can keep the existing _last
      | neededPadding (init pkt) == 0 = pkt
      | otherwise                     = init pkt ++ [(last pkt) {_last = Just (calcLast pkt)}]

-- | Test the padding inserter
paddingInserterTest :: forall n. 1 <= n => C.SNat n -> Property
paddingInserterTest C.SNat =
  propWithModelSingleDomain
    @C.System
    defExpectOptions
    (fmap fullPackets (Gen.list (Range.linear 0 100) genPackets))
    (C.exposeClockResetEnable model)
    (C.exposeClockResetEnable @C.System (ckt @n))
    (===)
  where
    ckt :: forall (dataWidth :: C.Nat) (dom :: C.Domain).
      C.HiddenClockResetEnable dom
      => 1 <= dataWidth
      => C.KnownNat dataWidth
      => Circuit (PacketStream dom dataWidth ()) (PacketStream dom dataWidth ())
    ckt = paddingInserterC

    -- This generates the packets
    genPackets =
      PacketStreamM2S <$>
      genVec Gen.enumBounded <*>
      Gen.maybe Gen.enumBounded <*>
      Gen.enumBounded <*>
      Gen.enumBounded

-- We test the edge case dataWidth = 1,
-- a case where dataWidth divides 64,
-- a case where dataWidth does not divide 64 and is less than 22 (= ceil(64/3)),
-- a case where dataWidth is more than 22, which seems to be the most fragile, and
-- a case where dataWidth is more than 64.
prop_paddinginserter_d1, prop_paddinginserter_d4, prop_paddinginserter_d13, prop_paddinginserter_d37, prop_paddinginserter_d70 :: Property
prop_paddinginserter_d1  = paddingInserterTest C.d1
prop_paddinginserter_d4  = paddingInserterTest C.d4
prop_paddinginserter_d13 = paddingInserterTest C.d13
prop_paddinginserter_d37 = paddingInserterTest C.d37
prop_paddinginserter_d70 = paddingInserterTest C.d70

tests :: TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1_000))
  $(testGroupGenerator)
