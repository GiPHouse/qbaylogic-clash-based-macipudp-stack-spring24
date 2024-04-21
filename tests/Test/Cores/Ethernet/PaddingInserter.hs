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


genVec :: (C.KnownNat n, 1 <= n) => Gen a -> Gen (C.Vec n a)
genVec gen = sequence (C.repeat gen)

model
  :: forall (dataWidth :: C.Nat).
  1 <= dataWidth
  => C.KnownNat dataWidth
  => [PacketStreamM2S dataWidth ()]
  -> [PacketStreamM2S dataWidth ()]
model fragments = concatMap (mergePackets . setLasts . insertPadding) $ chunkByPacket $ splitPackets fragments
  where
    insertPadding pkts = pkts ++ replicate (paddingNeeded pkts) padding
    paddingNeeded pkts = max 0 (64 - length pkts)
    padding = PacketStreamM2S {_data = C.repeat 0, _last = Nothing, _meta = (), _abort = False}
    setLasts pkts = map (\pkt -> pkt{_last = Nothing}) (init pkts) ++ [(last pkts){_last = Just 0}]

-- | Test the padding inserter
paddingInserterTest :: forall n. 1 <= n => C.SNat n -> Property
paddingInserterTest C.SNat =
  propWithModelSingleDomain
    @C.System
    defExpectOptions
    (fmap (cleanPackets . fullPackets) (Gen.list (Range.linear 0 100) genPackets))
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
-- a case where dataWidth does not divide 64, and
-- a case where dataWidth is more than 64.
prop_paddinginserter_d1, prop_paddinginserter_d4, prop_paddinginserter_d37, prop_paddinginserter_d70 :: Property
prop_paddinginserter_d1  = paddingInserterTest C.d1
prop_paddinginserter_d4  = paddingInserterTest C.d4
prop_paddinginserter_d37 = paddingInserterTest C.d37
prop_paddinginserter_d70 = paddingInserterTest C.d70

tests :: TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1_000))
  $(testGroupGenerator)
