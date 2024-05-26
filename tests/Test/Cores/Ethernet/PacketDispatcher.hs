{-# language FlexibleContexts #-}
{-# language NumericUnderscores #-}
{-# language RecordWildCards #-}

module Test.Cores.Ethernet.PacketDispatcher where

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
import Protocols.Hedgehog

-- Me
import Protocols.Extra.PacketStream
import Protocols.Extra.PacketStream.PacketDispatcher

genVec :: (C.KnownNat n, 1 <= n) => Gen a -> Gen (C.Vec n a)
genVec gen = sequence (C.repeat gen)

-- | Tests that the packet dispatcher works correctly with one sink that accepts
-- all packets; essentially an id test.
prop_packetdispatcher_id :: Property
prop_packetdispatcher_id = makePropPacketDispatcher C.d4
  ((const True :: Int -> Bool) C.:> C.Nil)

-- | Tests the packet dispatcher for a data width of four bytes and three
-- overlapping but incomplete dispatch functions, effectively testing whether
-- the circuit sends input to the first allowed output channel and drops input
-- if there are none.
prop_packetdispatcher :: Property
prop_packetdispatcher = makePropPacketDispatcher C.d4 fs
  where
    fs :: C.Vec 3 (C.Index 4 -> Bool)
    fs =
      (>= 3) C.:>
      (>= 2) C.:>
      (>= 1) C.:>
      C.Nil

-- | Generic test function for the packet dispatcher, testing for all data widths,
-- dispatch functions, and some meta types
makePropPacketDispatcher
  :: forall (p :: C.Nat) (dataWidth :: C.Nat) (a :: C.Type)
   . ( C.KnownNat p
     , 1 <= p
     , C.KnownNat dataWidth
     , 1 <= dataWidth
     , TestType a
     , Bounded a
     , Enum a
     )
  => C.SNat dataWidth
  -> C.Vec p (a -> Bool)
  -> Property
makePropPacketDispatcher _ fs = idWithModelSingleDomain @C.System
  defExpectOptions
  (Gen.list (Range.linear 0 100) genPackets)
  (C.exposeClockResetEnable (model 0))
  (C.exposeClockResetEnable (packetDispatcherC fs))
  where
    model :: C.Index p -> [PacketStreamM2S dataWidth a] -> C.Vec p [PacketStreamM2S dataWidth a]
    model _ [] = pure []
    model i (y : ys)
      | (fs C.!! i) (_meta y) = let next = model 0 ys in C.replace i (y : (next C.!! i)) next
      | i < maxBound = model (i + 1) (y : ys)
      | otherwise = model 0 ys

    -- TODO use util function from client review branch
    genPackets =
      PacketStreamM2S <$>
      genVec @dataWidth Gen.enumBounded <*>
      Gen.maybe Gen.enumBounded <*>
      Gen.enumBounded <*>
      Gen.enumBounded

tests :: TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1_000))
  $(testGroupGenerator)
