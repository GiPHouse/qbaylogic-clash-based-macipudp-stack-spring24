{-# language FlexibleContexts #-}
{-# language NumericUnderscores #-}
{-# language RecordWildCards #-}

module Test.Protocols.Extra.PacketStream.DelayCircuit where
-- base
import Prelude

-- clash-prelude
import Clash.Prelude qualified as C

-- hedgehog
import Hedgehog as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

-- tasty
import Test.Tasty
import Test.Tasty.Hedgehog ( HedgehogTestLimit(HedgehogTestLimit) )
import Test.Tasty.Hedgehog.Extra ( testProperty )
import Test.Tasty.TH ( testGroupGenerator )

-- clash-protocols
import Protocols ( Circuit )
import Protocols.Extra.PacketStream ( PacketStream, PacketStreamM2S(..) )
import Protocols.Extra.PacketStream.DelayCircuit ( delayPacketStreamC )
import Protocols.Hedgehog ( defExpectOptions, idWithModelSingleDomain )
import Test.Protocols.Extra.PacketStream ( makeValid )

genVec :: (C.KnownNat n, 1 C.<= n) => Gen a -> Gen (C.Vec n a)
genVec gen = sequence (C.repeat gen)


genWord :: Gen (PacketStreamM2S 4 ())
genWord =  PacketStreamM2S <$>
              genVec Gen.enumBounded <*>
              Gen.maybe Gen.enumBounded <*>
              Gen.enumBounded <*>
              Gen.enumBounded

genPackets :: Range Int -> Gen [PacketStreamM2S 4 ()]
genPackets range = makeValid <$> Gen.list range genWord

-- | test for id
prop_delay_circuit_id :: Property
prop_delay_circuit_id =
  idWithModelSingleDomain
    @C.System
    defExpectOptions
    (genPackets (Range.linear 0 100))
    (C.exposeClockResetEnable id)
    (C.exposeClockResetEnable ckt)
 where
  ckt :: C.HiddenClockResetEnable C.System => Circuit (PacketStream C.System 4 ()) (PacketStream C.System 4 ())
  ckt = delayPacketStreamC C.d5


tests :: TestTree
tests =
    localOption (mkTimeout 30_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1_000))
  $(testGroupGenerator)
