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
import Test.Cores.Ethernet.Util as U

genVec :: (C.KnownNat n, 1 <= n) => Gen a -> Gen (C.Vec n a)
genVec gen = sequence (C.repeat gen)

genPackets :: Gen (PacketStreamM2S 4 ())
genPackets =  PacketStreamM2S <$>
              genVec Gen.enumBounded <*>
              Gen.maybe Gen.enumBounded <*>
              Gen.enumBounded <*>
              Gen.enumBounded

equal :: [Maybe (PacketStreamM2S 4 ())] -> [Maybe (PacketStreamM2S 4 ())] -> Bool
equal a b = catMaybes a == catMaybes b

prop_packetBuffer_id :: Property
prop_packetBuffer_id = property $ do

  let ckt = exposeClockResetEnable (packetBufferC d16) systemClockGen resetGen enableGen

      noGaps :: [Maybe (PacketStreamM2S 4())] -> Bool
      noGaps (Just (PacketStreamM2S { _last = Nothing }):Nothing:_) = False
      noGaps (_:xs) = noGaps xs
      noGaps [] = True

      gen = U.fullPackets <$> Gen.list (Range.linear 0 100) genPackets

  (packets :: [PacketStreamM2S 4 ()]) <- H.forAll gen
 
  let packetBufferSize = d16
      cfg = SimulationConfig 1 (2 * snatToNum packetBufferSize) True
      sim = simulateC ckt cfg

      circuitResult = sim (Just <$> packets)

  assert $ noGaps circuitResult 

-- Fails ~SOMETIMES~ 
-- prop_packetBuffer_dropPackets :: Property
-- prop_packetBuffer_dropPackets = 
--   propWithModelMaybeControlSingleDomain 
--   @C.System
--   defExpectOptions
--   ((Prelude.++) <$>  somePackets <*> ((Prelude.++) <$> bigPacket <*> somePackets) )
--   (C.exposeClockResetEnable model)              -- Desired behaviour of Circuit
--   (C.exposeClockResetEnable ckt)
--   (prop)
--     where
--       somePackets = U.fullPackets <$> Gen.list (Range.linear 0 20) genPackets
--       bigPacket = U.fullPackets <$> lastToNothing <$> Gen.list (Range.linear 151 151) genPackets

--       ckt :: forall  (dom :: C.Domain).
--         C.HiddenClockResetEnable dom
--         => Circuit (PacketStream dom 4 ()) (PacketStream dom 4 ())
--       ckt = packetBufferC d7

--       model :: [PacketStreamM2S 4 ()] -> [Maybe (PacketStreamM2S 4 ())]
--       model xs = Just <$> (dropLargePackets 7 xs)

--       prop modelResult circuitResult = assert $ equal modelResult circuitResult

--       lastToNothing :: [PacketStreamM2S 4 ()] -> [PacketStreamM2S 4 ()]
--       lastToNothing list = setLast <$> list
--         where 
--           setLast word = word {_last = Nothing}

--       dropLargePackets :: Int -> [PacketStreamM2S 4 ()] -> [PacketStreamM2S 4 ()]
--       dropLargePackets size wordlist = Prelude.concat $ Prelude.reverse $ Prelude.filter fitts $ splitOnLast wordlist [] []
--         where 
--           splitOnLast :: [PacketStreamM2S 4 ()] -> [PacketStreamM2S 4 ()] -> [[PacketStreamM2S 4 ()]] -> [[PacketStreamM2S 4 ()]]
--           splitOnLast (x:xs) packet list = case (x:xs) of 
--             (PacketStreamM2S { _last = Nothing } : bs ) -> splitOnLast bs (x : packet) list
--             (PacketStreamM2S { _last = Just _ }  : bs ) -> splitOnLast bs [] ((Prelude.reverse (x : packet)) : list)
--           splitOnLast [] [] list = list 
--           splitOnLast [] packet list = (Prelude.reverse packet) : list

--           fitts :: [PacketStreamM2S 4 ()] -> Bool
--           fitts l = (Prelude.length l) <=  (2 Prelude.^ size) 

tests :: TestTree
tests =
    localOption (mkTimeout 30_000_000 {- 30 seconds -})
  $ localOption (HedgehogTestLimit (Just 1000))
  $(testGroupGenerator)
