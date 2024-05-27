{-# language FlexibleContexts #-}
{-# language NumericUnderscores #-}
{-# language RecordWildCards #-}

module Test.Protocols.Extra.PacketStream.PacketBuffer where

-- base
import Prelude
-- clash-prelude
import Clash.Prelude hiding ( drop, take, undefined, (++) )
import Clash.Prelude qualified as C
import Data.Int ( Int16 )

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
import Protocols
import Protocols.Hedgehog

-- Me
import Protocols.Extra.PacketStream
import Protocols.Extra.PacketStream.PacketBuffer ( overflowDropPacketBufferC, packetBufferC )
import Test.Protocols.Extra.PacketStream.Extra as U

genVec :: (C.KnownNat n, 1 C.<= n) => Gen a -> Gen (C.Vec n a)
genVec gen = sequence (C.repeat gen)

-- | generate a "clean" packet: a packet without an abort
genCleanWord :: Gen (PacketStreamM2S 4 Int16)
genCleanWord =  PacketStreamM2S <$>
                genVec Gen.enumBounded <*>
                pure Nothing <*>
                Gen.enumBounded <*>
                pure False

genWord :: Gen (PacketStreamM2S 4 Int16)
genWord =  PacketStreamM2S <$>
              genVec Gen.enumBounded <*>
              Gen.maybe Gen.enumBounded <*>
              Gen.enumBounded <*>
              Gen.enumBounded

genPackets :: Range Int -> Gen [PacketStreamM2S 4 Int16]
genPackets range = makeValid <$> Gen.list range genWord

isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (x:xs) (y:ys)
    | x == y    = isSubsequenceOf xs ys
    | otherwise = isSubsequenceOf xs (y:ys)

-- | test for id and proper dropping of aborted packets
prop_packetBuffer_id :: Property
prop_packetBuffer_id =
  idWithModelSingleDomain
    @C.System
    defExpectOptions
    (genPackets (Range.linear 0 100))
    (C.exposeClockResetEnable dropAbortedPackets)
    (C.exposeClockResetEnable ckt)
 where
  ckt :: HiddenClockResetEnable System => Circuit (PacketStream System 4 Int16) (PacketStream System 4 Int16)
  ckt = packetBufferC d12 d12

  -- test for id with a small buffer to ensure backpressure is tested
prop_packetBuffer_small_buffer_id :: Property
prop_packetBuffer_small_buffer_id =
  idWithModelSingleDomain
    @C.System
    defExpectOptions
    (genValidPackets (Range.linear 0 10) (Range.linear 0 31) genCleanWord)
    (C.exposeClockResetEnable dropAbortedPackets)
    (C.exposeClockResetEnable ckt)
 where
  ckt :: HiddenClockResetEnable System => Circuit (PacketStream System 4 Int16) (PacketStream System 4 Int16)
  ckt = packetBufferC d5 d5

-- | test to check if there are no gaps inside of packets
prop_packetBuffer_no_gaps :: Property
prop_packetBuffer_no_gaps = property $ do
  let packetBufferSize = d12
      maxInputSize = 50
      ckt = exposeClockResetEnable (packetBufferC packetBufferSize packetBufferSize) systemClockGen resetGen enableGen
      gen = genPackets (Range.linear 0 100)

  packets :: [PacketStreamM2S 4 Int16] <- H.forAll gen

  let packetSize = 2 Prelude.^ snatToInteger packetBufferSize
      cfg = SimulationConfig 1 (2 * packetSize) False
      cktResult = simulateC ckt cfg (Just <$> packets)

  assert $ noGaps $ take (5*maxInputSize) cktResult

  where
    noGaps :: [Maybe (PacketStreamM2S 4 Int16)] -> Bool
    noGaps (Just (PacketStreamM2S{_last = Nothing}):Nothing:_) = False
    noGaps (_:xs) = noGaps xs
    noGaps _ = True

-- | test for id and proper dropping of aborted packets
prop_overFlowDrop_packetBuffer_id :: Property
prop_overFlowDrop_packetBuffer_id =
  idWithModelSingleDomain
    @C.System
    defExpectOptions
    (genPackets (Range.linear 0 100))
    (C.exposeClockResetEnable dropAbortedPackets)
    (C.exposeClockResetEnable ckt)
 where
  ckt :: HiddenClockResetEnable System => Circuit (PacketStream System 4 Int16) (PacketStream System 4 Int16)
  ckt = fromPacketStream |> overflowDropPacketBufferC d12 d12


-- | test for proper dropping when full
prop_overFlowDrop_packetBuffer_drop :: Property
prop_overFlowDrop_packetBuffer_drop =
  idWithModelSingleDomain
    @C.System
    (ExpectOptions 50 (Just 1_000) 30 False)
    -- make sure the timeout is long as the packetbuffer can be quiet for a while while dropping
    (liftA3 (\a b c -> a ++ b ++ c)  genSmall genBig genSmall)
    (C.exposeClockResetEnable model)
    (C.exposeClockResetEnable ckt)
 where
  bufferSize = d5

  ckt :: HiddenClockResetEnable System => Circuit (PacketStream System 4 Int16) (PacketStream System 4 Int16)
  ckt = fromPacketStream |> overflowDropPacketBufferC bufferSize bufferSize

  model :: [PacketStreamM2S 4 Int16] -> [PacketStreamM2S 4 Int16]
  model packets = Prelude.concat $ take 1 packetChunk ++ drop 2 packetChunk
    where
      packetChunk = chunkByPacket packets

  genSmall = genValidPacket (Range.linear 1 5) genCleanWord
  genBig = genValidPacket (Range.linear 33 50) genCleanWord

-- | test for id using a small metabuffer to ensure backpressure using the metabuffer is tested
prop_packetBuffer_small_metaBuffer :: Property
prop_packetBuffer_small_metaBuffer =
  idWithModelSingleDomain
    @C.System
    defExpectOptions
    (genPackets (Range.linear 0 100))
    (C.exposeClockResetEnable dropAbortedPackets)
    (C.exposeClockResetEnable ckt)
 where
  ckt :: HiddenClockResetEnable System => Circuit (PacketStream System 4 Int16) (PacketStream System 4 Int16)
  ckt = packetBufferC d12 d2

tests :: TestTree
tests =
    localOption (mkTimeout 30_000_000 {- 30 seconds -})
  $ localOption (HedgehogTestLimit (Just 1_000))
  $(testGroupGenerator)
