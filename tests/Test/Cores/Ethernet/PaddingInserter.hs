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
  :: forall (dataWidth :: C.Nat)
   . 1 <= dataWidth
  => C.KnownNat dataWidth
  => Int
  -> [PacketStreamM2S dataWidth ()]
  -> [PacketStreamM2S dataWidth ()]
model padBytes fragments = concatMap (mergePackets . setLasts . insertPadding) $ chunkByPacket $ splitPackets fragments
  where
    insertPadding pkts = pkts ++ replicate (paddingNeeded pkts) padding
    paddingNeeded pkts = max 0 (padBytes - length pkts)
    padding = PacketStreamM2S {_data = C.repeat 0, _last = Nothing, _meta = (), _abort = False}
    setLasts pkts = map (\pkt -> pkt{_last = Nothing}) (init pkts) ++ [(last pkts){_last = Just 0}]

-- | Test the padding inserter.
paddingInserterTest
  :: forall n p. C.KnownNat n => C.KnownNat p => 1 <= n => 1 <= p => C.SNat n -> C.SNat p -> Property
paddingInserterTest _ padBytes =
  propWithModelSingleDomain
    @C.System
    defExpectOptions
    (fmap (cleanPackets . fullPackets) (Gen.list (Range.linear 0 100) genPackets))
    (C.exposeClockResetEnable (model $ C.natToNum @p))
    (C.exposeClockResetEnable @C.System (ckt @n))
    (===)
  where
    ckt :: forall (dataWidth :: C.Nat) (dom :: C.Domain).
      C.HiddenClockResetEnable dom
      => 1 <= dataWidth
      => C.KnownNat dataWidth
      => Circuit (PacketStream dom dataWidth ()) (PacketStream dom dataWidth ())
    ckt = paddingInserterC padBytes

    -- This generates the packets
    genPackets =
      PacketStreamM2S <$>
      genVec Gen.enumBounded <*>
      Gen.maybe Gen.enumBounded <*>
      Gen.enumBounded <*>
      Gen.enumBounded

-- We test the edge case dataWidth = padBytes = 1,
-- a case where dataWidth divides padBytes,
-- a case where dataWidth does not divide padBytes, and
-- a case where dataWidth is more than padBytes.
prop_paddinginserter_d1, prop_paddinginserter_d4, prop_paddinginserter_d5, prop_paddinginserter_d50 :: Property
prop_paddinginserter_d1  = paddingInserterTest C.d1 C.d1
prop_paddinginserter_d4  = paddingInserterTest C.d4 C.d46
prop_paddinginserter_d5 = paddingInserterTest C.d5 C.d46
prop_paddinginserter_d50 = paddingInserterTest C.d50 C.d46

tests :: TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1_000))
  $(testGroupGenerator)
