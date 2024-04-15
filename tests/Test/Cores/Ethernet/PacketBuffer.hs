{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Cores.Ethernet.PacketBuffer where

-- base
import Prelude
import Data.Maybe
import qualified Data.List as L

-- clash-prelude
import qualified Clash.Prelude as C
import Clash.Prelude hiding (undefined, (++), drop, take)

-- hedgehog
import Hedgehog as H
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
import Clash.Cores.Ethernet.PacketBuffer
import Test.Cores.Ethernet.Util as U

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

  -- test for id and proper dropping of aborted packets
prop_packetBuffer_id_small_buffer :: Property
prop_packetBuffer_id_small_buffer =
  propWithModelSingleDomain
    @C.System
    defExpectOptions
    (Prelude.concat <$> (fmap U.fullPackets <$> genListofLists))
    (C.exposeClockResetEnable dropAbortedPackets)
    (C.exposeClockResetEnable ckt)
    (===)
 where
  ckt :: HiddenClockResetEnable System => Circuit (PacketStream System 4 ()) (PacketStream System 4 ())
  ckt = packetBufferC d5

  genListofLists = Gen.list (Range.linear 0 6) $ Gen.list (Range.linear 0 32) genNotLasts
   
  genNotLasts :: Gen (PacketStreamM2S 4 ())
  genNotLasts =  PacketStreamM2S <$>
    genVec Gen.enumBounded <*>
    pure Nothing <*>
    Gen.enumBounded <*>
    pure False


prop_packetBuffer_no_gaps :: Property
prop_packetBuffer_no_gaps = property $ do
  let packetBufferSize = d12
      maxInputSize = 50
      ckt = exposeClockResetEnable (packetBufferC packetBufferSize) systemClockGen resetGen enableGen
      gen = U.fullPackets <$> Gen.list (Range.linear 0 maxInputSize) genPackets

  packets :: [PacketStreamM2S 4 ()] <- H.forAll gen

  let packetSize = 2 Prelude.^ snatToNum packetBufferSize
      cfg = SimulationConfig 1 (2 * packetSize) False
      cktResult = simulateC ckt cfg (Just <$> packets)

  assert $ noGaps $ take (5*maxInputSize) cktResult

  where
    noGaps :: [Maybe (PacketStreamM2S 4 ())] -> Bool
    noGaps (Just (PacketStreamM2S{_last = Nothing}):Nothing:_) = False
    noGaps (_:xs) = noGaps xs
    noGaps _ = True

prop_csignal_packetBuffer_id :: Property
prop_csignal_packetBuffer_id =
  propWithModelSingleDomain
    @C.System
    defExpectOptions
    (U.fullPackets <$> Gen.list (Range.linear 0 100) genPackets)
    (C.exposeClockResetEnable dropAbortedPackets)
    (C.exposeClockResetEnable ckt)
    (===)
 where
  ckt :: HiddenClockResetEnable System => Circuit (PacketStream System 4 ()) (PacketStream System 4 ())
  ckt = fromPacketStream |> cSignalPacketBufferC d12

prop_csignal_packetBuffer_drop :: Property
prop_csignal_packetBuffer_drop =
  propWithModelSingleDomain
    @C.System
    (ExpectOptions 50 (Just 1_000) 30 False)
    (liftA3 (\a b c -> a ++ b ++ c)  genSmall genBig genSmall)
    (C.exposeClockResetEnable model)
    (C.exposeClockResetEnable ckt)
    (===)
 where
  bufferSize = d5

  ckt :: HiddenClockResetEnable System => Circuit (PacketStream System 4 ()) (PacketStream System 4 ())
  ckt = fromPacketStream |> cSignalPacketBufferC bufferSize

  model :: [PacketStreamM2S 4 ()] -> [PacketStreamM2S 4 ()]
  model packets = Prelude.concat $ take 1 packetChunk ++ drop 2 packetChunk
    where
      packetChunk = chunkByPacket packets

  genSmall = U.fullPackets <$> Gen.list (Range.linear 1 5) genClean
  genBig = U.fullPackets <$> Gen.list (Range.linear 33 50) genClean

  genClean :: Gen (PacketStreamM2S 4 ())
  genClean =  PacketStreamM2S <$>
      genVec Gen.enumBounded <*>
      pure Nothing <*>
      Gen.enumBounded <*>
      pure False


tests :: TestTree
tests =
    localOption (mkTimeout 30_000_000 {- 30 seconds -})
  $ localOption (HedgehogTestLimit (Just 10_000))
  $(testGroupGenerator)
