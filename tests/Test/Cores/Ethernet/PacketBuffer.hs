{-# language FlexibleContexts #-}
{-# language NumericUnderscores #-}
{-# language RecordWildCards #-}

module Test.Cores.Ethernet.PacketBuffer where

-- base
import Data.Maybe
import Prelude

-- clash-prelude
import qualified Clash.Prelude as C
import Clash.Prelude hiding (undefined, (++), drop, take)

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
import Clash.Cores.Ethernet.PacketBuffer
import Clash.Cores.Ethernet.PacketStream
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

prop_packetBuffer_drop :: Property
prop_packetBuffer_drop = property $ do
  let packetBufferSize = d5
      ckt = exposeClockResetEnable (packetBufferC packetBufferSize) systemClockGen resetGen enableGen
      gen = U.fullPackets <$> Gen.list (Range.linear 0 100) genPackets

  packets :: [PacketStreamM2S 4 ()] <- H.forAll gen

  let packetSize = 2 Prelude.^ snatToNum packetBufferSize
      cfg = SimulationConfig 1 (2 * packetSize) False
      cktResult = simulateC ckt cfg (Just <$> packets)

  let cktByPacket = chunkByPacket $ catMaybes cktResult
      genByPacket = chunkByPacket packets
  diff cktByPacket isSubsequenceOf genByPacket


prop_packetBuffer_dropLarge :: Property
prop_packetBuffer_dropLarge =
  propWithModelSingleDomain
    @C.System
    (ExpectOptions 50 (Just $ 1_000) 30 False)
    (liftA3 (\a b c -> a ++ b ++ c) genSmall genBig genSmall)
    (C.exposeClockResetEnable model)
    (C.exposeClockResetEnable ckt)
    (===)
 where
  bufferSize = d5
  genSmallSize = 5
  genBigSize = 40

  ckt :: HiddenClockResetEnable System => Circuit (PacketStream System 4 ()) (PacketStream System 4 ())
  ckt = packetBufferC bufferSize

  model :: [PacketStreamM2S 4 ()] -> [PacketStreamM2S 4 ()]
  model packets = take genSmallSize packets ++ drop (genSmallSize + genBigSize) packets
  -- model = id

  genSmall = U.fullPackets <$> Gen.list (Range.linear genSmallSize genSmallSize) genNotLasts
  genBig = U.fullPackets <$> Gen.list (Range.linear genBigSize genBigSize) genNotLasts

  genNotLasts :: Gen (PacketStreamM2S 4 ())
  genNotLasts =  PacketStreamM2S <$>
            genVec Gen.enumBounded <*>
            pure Nothing <*>
            Gen.enumBounded <*>
            Gen.enumBounded


prop_packetBuffer_id :: Property
prop_packetBuffer_id =
  propWithModelSingleDomain
    @C.System
    defExpectOptions
    (U.fullPackets <$> Gen.list (Range.linear 0 100) genPackets)
    (C.exposeClockResetEnable id)
    (C.exposeClockResetEnable ckt)
    (===)
 where
  ckt :: HiddenClockResetEnable System => Circuit (PacketStream System 4 ()) (PacketStream System 4 ())
  ckt = packetBufferC d12

tests :: TestTree
tests =
    localOption (mkTimeout 30_000_000 {- 30 seconds -})
  $ localOption (HedgehogTestLimit (Just 10_000))
  $(testGroupGenerator)
