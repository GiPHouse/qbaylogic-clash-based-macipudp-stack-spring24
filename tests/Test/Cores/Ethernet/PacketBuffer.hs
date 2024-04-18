{-# language FlexibleContexts #-}
{-# language NumericUnderscores #-}
{-# language RecordWildCards #-}

module Test.Cores.Ethernet.PacketBuffer where

-- base
import Prelude
-- clash-prelude
import Clash.Prelude hiding ( drop, take, undefined, (++) )
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
import Protocols
import Protocols.Hedgehog

-- Me
import Clash.Cores.Ethernet.PacketBuffer ( overflowDropPacketBufferC, packetBufferC )
import Clash.Cores.Ethernet.PacketStream
import Test.Cores.Ethernet.Util as U

-- | generate a "clean" packet: a packet without an abort
genCleanPacket :: Range Int -> Gen [PacketStreamM2S 4 ()]
genCleanPacket range = fmap fullPackets $
    Gen.list range $ PacketStreamM2S <$>
    genVec Gen.enumBounded <*>
    pure Nothing <*>
    Gen.enumBounded <*>
    pure False

genVec :: (C.KnownNat n, 1 <= n) => Gen a -> Gen (C.Vec n a)
genVec gen = sequence (C.repeat gen)

genPackets :: Gen (PacketStreamM2S 4 ())
genPackets =  PacketStreamM2S <$>
              genVec Gen.enumBounded <*>
              Gen.maybe Gen.enumBounded <*>
              Gen.enumBounded <*>
              Gen.enumBounded

isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (x:xs) (y:ys)
    | x == y    = isSubsequenceOf xs ys
    | otherwise = isSubsequenceOf xs (y:ys)

-- | test for id and proper dropping of aborted packets
prop_packetBuffer_id :: Property
prop_packetBuffer_id =
  propWithModelSingleDomain
    @C.System
    defExpectOptions
    (U.fullPackets <$> Gen.list (Range.linear 0 100) genPackets)
    (C.exposeClockResetEnable dropAbortedPackets)
    (C.exposeClockResetEnable ckt)
    (===)
 where
  ckt :: HiddenClockResetEnable System => Circuit (PacketStream System 4 ()) (PacketStream System 4 ())
  ckt = packetBufferC d12

  -- test for id and proper dropping of aborted packets, with a small buffer to ensure
  -- backpressure is tested
prop_packetBuffer_id_small_buffer :: Property
prop_packetBuffer_id_small_buffer =
  idWithModelSingleDomain
    @C.System
    defExpectOptions
    gen
    (C.exposeClockResetEnable dropAbortedPackets)
    (C.exposeClockResetEnable ckt)
 where
  ckt :: HiddenClockResetEnable System => Circuit (PacketStream System 4 ()) (PacketStream System 4 ())
  ckt = packetBufferC d5

  gen :: Gen [PacketStreamM2S 4 ()]
  gen = Prelude.concat <$> Gen.list (Range.linear 0 10) (genCleanPacket (Range.linear 0 31))

-- | test to check if there are no gaps inside of packets
prop_packetBuffer_no_gaps :: Property
prop_packetBuffer_no_gaps = property $ do
  let packetBufferSize = d12
      maxInputSize = 50
      ckt = exposeClockResetEnable (packetBufferC packetBufferSize) systemClockGen resetGen enableGen
      gen = U.fullPackets <$> Gen.list (Range.linear 0 maxInputSize) genPackets

  packets :: [PacketStreamM2S 4 ()] <- H.forAll gen

  let packetSize = 2 Prelude.^ snatToInteger packetBufferSize
      cfg = SimulationConfig 1 (2 * packetSize) False
      cktResult = simulateC ckt cfg (Just <$> packets)

  assert $ noGaps $ take (5*maxInputSize) cktResult

  where
    noGaps :: [Maybe (PacketStreamM2S 4 ())] -> Bool
    noGaps (Just (PacketStreamM2S{_last = Nothing}):Nothing:_) = False
    noGaps (_:xs) = noGaps xs
    noGaps _ = True

-- | test for id and proper dropping of aborted packets
prop_overflowDropPacketBufferC_id :: Property
prop_overflowDropPacketBufferC_id =
  propWithModelSingleDomain
    @C.System
    defExpectOptions
    (U.fullPackets <$> Gen.list (Range.linear 0 100) genPackets)
    (C.exposeClockResetEnable dropAbortedPackets)
    (C.exposeClockResetEnable ckt)
    (===)
 where
  ckt :: HiddenClockResetEnable System => Circuit (PacketStream System 4 ()) (PacketStream System 4 ())
  ckt = fromPacketStream |> overflowDropPacketBufferC d12


-- | test for proper dropping when full
prop_overflowDropPacketBufferC_drop :: Property
prop_overflowDropPacketBufferC_drop =
  propWithModelSingleDomain
    @C.System
    (ExpectOptions 50 (Just 1_000) 30 False)
    -- make sure the timeout is long as the packetbuffer can be quiet for a while while dropping
    (liftA3 (\a b c -> a ++ b ++ c)  genSmall genBig genSmall)
    (C.exposeClockResetEnable model)
    (C.exposeClockResetEnable ckt)
    (===)
 where
  bufferSize = d5

  ckt :: HiddenClockResetEnable System => Circuit (PacketStream System 4 ()) (PacketStream System 4 ())
  ckt = fromPacketStream |> overflowDropPacketBufferC bufferSize

  model :: [PacketStreamM2S 4 ()] -> [PacketStreamM2S 4 ()]
  model packets = Prelude.concat $ take 1 packetChunk ++ drop 2 packetChunk
    where
      packetChunk = chunkByPacket packets

  genSmall = genCleanPacket (Range.linear 1 5)
  genBig = genCleanPacket (Range.linear 33 50)

tests :: TestTree
tests =
    localOption (mkTimeout 30_000_000 {- 30 seconds -})
  $ localOption (HedgehogTestLimit (Just 1_000))
  $(testGroupGenerator)
