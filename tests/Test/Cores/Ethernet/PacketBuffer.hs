{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Cores.Ethernet.PacketBuffer where

-- base
import Prelude
import Data.Maybe

-- clash-prelude
import qualified Clash.Prelude as C
import Clash.Prelude hiding (undefined)

-- hedgehog
import Hedgehog
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

prop_packetBuffer_id :: Property
prop_packetBuffer_id = 
  propWithModelMaybeControlSingleDomain 
  @C.System
  defExpectOptions
  ((Prelude.++) <$> Gen.list (Range.linear 0 100) genPackets <*> Gen.list (Range.linear 1 1) genLastPackets)
  (C.exposeClockResetEnable model)              -- Desired behaviour of Circuit
  (C.exposeClockResetEnable ckt)
  (property)
    where
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

      ckt :: forall  (dom :: C.Domain).
        C.HiddenClockResetEnable dom
        => Circuit (PacketStream dom 4 ()) (PacketStream dom 4 ())
      
      ckt = packetBufferC d16

      model :: [PacketStreamM2S 4 ()] -> [Maybe (PacketStreamM2S 4 ())]
      model xs = Just <$> xs

      property modelResult circuitResult = assert $ noGaps circuitResult Prelude.&& equal modelResult circuitResult
        where
          equal a b = catMaybes a == catMaybes b

      noGaps :: [Maybe (PacketStreamM2S 4())] -> Bool
      noGaps (Just (PacketStreamM2S { _last = Nothing }):Nothing:_) = False
      noGaps (_:xs) = noGaps xs
      noGaps [] = True

prop_packetBuffer_dropPackets :: Property
prop_packetBuffer_dropPackets = 
  propWithModelMaybeControlSingleDomain 
  @C.System
  defExpectOptions
  ((Prelude.++) <$> ((Prelude.++) <$> somePackets <*> bigPacket) <*> somePackets)
  (C.exposeClockResetEnable model)              -- Desired behaviour of Circuit
  (C.exposeClockResetEnable ckt)
  (property)
    where
      somePackets = (Prelude.++) <$> Gen.list (Range.linear 0 20) genPackets <*> Gen.list (Range.linear 1 1) genLastPackets
      bigPacket = (Prelude.++) <$> Gen.list (Range.linear 100 100) genNotLastPackets <*> Gen.list (Range.linear 1 1) genLastPackets

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
        ((Just <$> Gen.enumBounded)) <*>
        Gen.enumBounded <*>
        Gen.enumBounded

      ckt :: forall  (dom :: C.Domain).
        C.HiddenClockResetEnable dom
        => Circuit (PacketStream dom 4 ()) (PacketStream dom 4 ())
      
      ckt = packetBufferC d8

      model :: [PacketStreamM2S 4 ()] -> [Maybe (PacketStreamM2S 4 ())]
      model xs = Just <$> xs

      dropLargePackets :: [PacketStreamM2S 4 ()] -> [PacketStreamM2S 4 ()]
      dropLargePackets words = Prelude.concat $ Prelude.filter large $ splitOnLast words [] []
        where 
          splitOnLast :: [PacketStreamM2S 4 ()] -> [PacketStreamM2S 4 ()] -> [[PacketStreamM2S 4 ()]] -> [[PacketStreamM2S 4 ()]]
          splitOnLast (x : xs ) packet list = case (x : xs) of 
            (PacketStreamM2S { _last = Nothing } : xs ) -> splitOnLast xs (packet Prelude.++ [x] ) list
            (PacketStreamM2S { _last = Just _ }  : xs ) -> splitOnLast xs [] (list Prelude.++ [packet])
          splitOnLast [] [] list = list 
          splitOnLast [] packet list = list Prelude.++ [packet]

          large :: [PacketStreamM2S 4 ()] -> Bool
          large l = (2 Prelude.^ 8) < (4 *(Prelude.length l))

      property modelResult circuitResult = assert $ noGaps circuitResult Prelude.&& equal modelResult circuitResult
        where
          equal a b = catMaybes a == catMaybes b

      noGaps :: [Maybe (PacketStreamM2S 4())] -> Bool
      noGaps (Just (PacketStreamM2S { _last = Nothing }):Nothing:_) = False
      noGaps (_:xs) = noGaps xs
      noGaps [] = True

tests :: TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1_000))
  $(testGroupGenerator)
