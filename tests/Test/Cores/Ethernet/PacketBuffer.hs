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
import Clash.Prelude hiding (undefined)

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
import Test.Cores.Ethernet.MaybeControl (propWithModelMaybeControlSingleDomain)

genVec :: (C.KnownNat n, 1 <= n) => Gen a -> Gen (C.Vec n a)
genVec gen = sequence (C.repeat gen)

dropLargePackets :: Int -> [PacketStreamM2S 4 ()] -> [PacketStreamM2S 4 ()]
dropLargePackets size words = Prelude.concat $ Prelude.reverse $ Prelude.filter (fitts size) $ splitOnLast words [] []
  where 
    splitOnLast :: [PacketStreamM2S 4 ()] -> [PacketStreamM2S 4 ()] -> [[PacketStreamM2S 4 ()]] -> [[PacketStreamM2S 4 ()]]
    splitOnLast (x : xs ) packet list = case (x : xs) of 
      (PacketStreamM2S { _last = Nothing } : xs ) -> splitOnLast xs (x : packet) list
      (PacketStreamM2S { _last = Just _ }  : xs ) -> splitOnLast xs [] ((Prelude.reverse (x : packet)) : list)
    splitOnLast [] [] list = list 
    splitOnLast [] packet list = (Prelude.reverse packet) : list

    fitts :: Int -> [PacketStreamM2S 4 ()] -> Bool
    fitts size l = (Prelude.length l) <=  (2 Prelude.^ size) 

prop_packetBuffer_id :: Property
prop_packetBuffer_id = property $ do

  let genPackets =
         PacketStreamM2S <$>
         genVec Gen.enumBounded <*>
         Gen.maybe Gen.enumBounded <*>
         Gen.enumBounded <*>
         Gen.enumBounded

      genLastPackets =
        PacketStreamM2S <$>
        genVec Gen.enumBounded <*>
        (Just <$> Gen.enumBounded) <*>
        Gen.enumBounded <*>
        Gen.enumBounded

      ckt = exposeClockResetEnable (packetBufferC d16) systemClockGen resetGen enableGen

      equal :: [Maybe (PacketStreamM2S 4())] -> [Maybe (PacketStreamM2S 4())] -> Bool
      equal a b = catMaybes a == catMaybes b

      noGaps :: [Maybe (PacketStreamM2S 4())] -> Bool
      noGaps (Just (PacketStreamM2S { _last = Nothing }):Nothing:_) = False
      noGaps (_:xs) = noGaps xs
      noGaps [] = True

  let n = 10_000
  let gen = (Prelude.++) <$> Gen.list (Range.linear 0 100) genPackets <*> Gen.list (Range.linear 1 1) genLastPackets

  (packets :: [PacketStreamM2S 4 ()]) <- H.forAll gen
  let packetBufferSize = d16
  let cfg = SimulationConfig 1 (2 * snatToNum packetBufferSize) True
  let sim = simulateC ckt cfg

  let circuitResult = sim (Just <$> packets)

  assert $ noGaps circuitResult 

prop_packetBuffer_dropLargePacket :: Property
prop_packetBuffer_dropLargePacket = 
  propWithModelMaybeControlSingleDomain 
  @C.System
  defExpectOptions
  (bigPacket )
  (C.exposeClockResetEnable model)              -- Desired behaviour of Circuit
  (C.exposeClockResetEnable ckt)
  (property)
    where
      bigPacket = (Prelude.++) <$> Gen.list (Range.linear 200 200) genNotLastPackets <*> Gen.list (Range.linear 1 1) genLastPackets
      genLastPackets =
        PacketStreamM2S <$>
        genVec Gen.enumBounded <*>
        (Just <$> Gen.enumBounded) <*>
        Gen.enumBounded <*>
        Gen.enumBounded

      genNotLastPackets = 
        PacketStreamM2S <$>
        genVec Gen.enumBounded <*>
        (pure Nothing) <*>
        Gen.enumBounded <*>
        Gen.enumBounded

      ckt :: forall  (dom :: C.Domain).
        C.HiddenClockResetEnable dom
        => Circuit (PacketStream dom 4 ()) (PacketStream dom 4 ())
      ckt = packetBufferC d7

      model :: [PacketStreamM2S 4 ()] -> [Maybe (PacketStreamM2S 4 ())]
      model xs = Just <$> (dropLargePackets 7 xs)

      property modelResult circuitResult = assert $ equal modelResult circuitResult
        where
          equal a b = catMaybes a == catMaybes b

prop_packetBuffer_dropPackets_withBackpressure :: Property
prop_packetBuffer_dropPackets_withBackpressure = 
  propWithModelMaybeControlSingleDomain 
  @C.System
  defExpectOptions
  ((Prelude.++) <$>  somePackets <*> ((Prelude.++) <$> bigPacket <*> somePackets) )
  (C.exposeClockResetEnable model)              -- Desired behaviour of Circuit
  (C.exposeClockResetEnable ckt)
  (property)
    where
      somePackets = (Prelude.++) <$> Gen.list (Range.linear 0 20) genPackets <*> Gen.list (Range.linear 1 1) genLastPackets
      bigPacket = (Prelude.++) <$> Gen.list (Range.linear 150 150) genNotLastPackets <*> Gen.list (Range.linear 1 1) genLastPackets

      genPackets =
         PacketStreamM2S <$>
         genVec Gen.enumBounded <*>
         Gen.maybe Gen.enumBounded <*>
         Gen.enumBounded <*>
         Gen.enumBounded

      genLastPackets =
        PacketStreamM2S <$>
        genVec Gen.enumBounded <*>
        (Just <$> Gen.enumBounded) <*>
        Gen.enumBounded <*>
        Gen.enumBounded

      genNotLastPackets = 
        PacketStreamM2S <$>
        genVec Gen.enumBounded <*>
        (pure Nothing) <*>
        Gen.enumBounded <*>
        Gen.enumBounded

      ckt :: forall  (dom :: C.Domain).
        C.HiddenClockResetEnable dom
        => Circuit (PacketStream dom 4 ()) (PacketStream dom 4 ())
      ckt = packetBufferC d7

      model :: [PacketStreamM2S 4 ()] -> [Maybe (PacketStreamM2S 4 ())]
      model xs = Just <$> (dropLargePackets 7 xs)

      property modelResult circuitResult = assert $ equal modelResult circuitResult
        where
          equal a b = catMaybes a == catMaybes b

tests :: TestTree
tests =
    localOption (mkTimeout 30_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1000))
  $(testGroupGenerator)
