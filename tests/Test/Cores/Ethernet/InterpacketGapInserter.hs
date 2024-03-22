{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Cores.Ethernet.InterpacketGapInserter where

-- base
import Prelude

import qualified Data.List as L

-- clash-prelude
import Clash.Prelude hiding (repeat)
import qualified Clash.Prelude as C
--import Clash.Prelude (type (<=))

-- hedgehog
import Hedgehog

-- tasty
import Test.Tasty
import Test.Tasty.Hedgehog (HedgehogTestLimit(HedgehogTestLimit))
import Test.Tasty.TH (testGroupGenerator)

-- clash-protocols
import Protocols

-- Me
import Clash.Cores.Ethernet.PacketStream
import Clash.Cores.Ethernet.InterpacketGapInserter

genVec :: (KnownNat n, 1 <= n) => Gen a -> Gen (Vec n a)
genVec gen = sequence (C.repeat gen)

fwdIn :: [Maybe (PacketStreamM2S 1 ())]
fwdIn = [
  Just (PacketStreamM2S (0xAB :> Nil) Nothing () False)
  , Just (PacketStreamM2S (0xCD :> Nil) (Just 0) () False)
  , Just (PacketStreamM2S (0x01 :> Nil) Nothing () False)
  , Just (PacketStreamM2S (0x01 :> Nil) Nothing () False)
  , Just (PacketStreamM2S (0x01 :> Nil) Nothing () False)
  , Just (PacketStreamM2S (0x01 :> Nil) Nothing () False)
  , Just (PacketStreamM2S (0x01 :> Nil) Nothing () False)
  , Just (PacketStreamM2S (0x01 :> Nil) Nothing () False)
  , Just (PacketStreamM2S (0x01 :> Nil) Nothing () False)
  , Just (PacketStreamM2S (0x01 :> Nil) Nothing () False)
  , Just (PacketStreamM2S (0x01 :> Nil) Nothing () False)
  , Just (PacketStreamM2S (0x01 :> Nil) Nothing () False)
  , Just (PacketStreamM2S (0x01 :> Nil) Nothing () False)
  , Just (PacketStreamM2S (0x01 :> Nil) Nothing () False)
  , Just (PacketStreamM2S (0x01 :> Nil) Nothing () False)] L.++ L.repeat Nothing

bwdIn :: [PacketStreamS2M]
bwdIn = fmap PacketStreamS2M (L.repeat True)

expectedFwdOut :: [Maybe (PacketStreamM2S 1 ())]
expectedFwdOut = [
    Just (PacketStreamM2S (0xAB :> Nil) Nothing () False)
  , Just (PacketStreamM2S (0xCD :> Nil) (Just 0) () False)]
  L.++ L.replicate 12 Nothing
  L.++ [Just (PacketStreamM2S (0x01 :> Nil) Nothing () False)]
  L.++ L.repeat Nothing

expectedBwdOut :: [PacketStreamS2M]
expectedBwdOut = fmap PacketStreamS2M ([True, True] L.++ L.replicate 12 False)

clk :: Clock System
clk = systemClockGen

rst :: Reset System
rst = systemResetGen

en :: Enable System
en = enableGen

fwdOut :: Signal System (Maybe (PacketStreamM2S 1 ()))
bwdOut :: Signal System PacketStreamS2M
(bwdOut, fwdOut) = toSignals ckt (fromList fwdIn, fromList bwdIn)
  where ckt = exposeClockResetEnable interpacketGapInserterC clk rst en

twelveCyclesNothingAfterLast :: Property
twelveCyclesNothingAfterLast = property $
  do L.map fst (sampleN 15 $ bundle (fwdOut, bwdOut)) /== expectedFwdOut

twelveCyclesBackpressureAfterLast :: Property
twelveCyclesBackpressureAfterLast = property $
  do L.map snd (sampleN 15 $ bundle (fwdOut, bwdOut)) /== expectedBwdOut

tests :: TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1_000))
  $(testGroupGenerator)
