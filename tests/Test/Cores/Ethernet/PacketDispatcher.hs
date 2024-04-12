{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Cores.Ethernet.PacketDispatcher where

-- base
import Prelude

-- clash-prelude
import qualified Clash.Prelude as C
import Clash.Prelude (type(<=))

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
import Protocols.Hedgehog

-- Me
import Clash.Cores.Ethernet.PacketStream
import Clash.Cores.Ethernet.PacketDispatcher

genVec :: (C.KnownNat n, 1 <= n) => Gen a -> Gen (C.Vec n a)
genVec gen = sequence (C.repeat gen)

-- | Tests the packet dispatcher for a data width of four bytes and three
-- overlapping but incomplete dispatch functions, effectively testing whether
-- the circuit sends input to the first allowed output channel and drops input
-- if there are none.
prop_packetdispatcher :: Property
prop_packetdispatcher = makePropPacketdispatcher C.d4 hs
  where
    hs :: C.Vec 3 (C.Index 4 -> Bool)
    hs =
      (>= 3) C.:>
      (>= 2) C.:>
      (>= 1) C.:>
      C.Nil

-- | Generic test function for the packet dispatcher, testing for all data widths,
-- dispatch functions, and some meta types
makePropPacketdispatcher
  :: forall (p :: C.Nat) (dataWidth :: C.Nat) a
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
makePropPacketdispatcher n fs = idWithModelSingleDomain @C.System
  defExpectOptions
  (Gen.list (Range.linear 0 100) genPackets)
  (C.exposeClockResetEnable (model n fs))
  (C.exposeClockResetEnable (packetDispatcherC fs))
  where
    model :: C.SNat n -> C.Vec p (a -> Bool) -> [PacketStreamM2S n a] -> C.Vec p [PacketStreamM2S n a]
    model _ gs = model' 0 where
      model' :: C.Index p -> [PacketStreamM2S n a] -> C.Vec p [PacketStreamM2S n a]
      model' _ [] = pure []
      model' i (y : ys)
        | (gs C.!! i) (_meta y) = let next = model' 0 ys in C.replace i (y : (next C.!! i)) next
        | i < maxBound = model' (i + 1) (y : ys)
        | otherwise = model' 0 ys

    -- TODO use util function from client review branch
    genPackets =
      PacketStreamM2S <$>
      genVec Gen.enumBounded <*>
      Gen.maybe Gen.enumBounded <*>
      Gen.enumBounded <*>
      Gen.enumBounded

tests :: TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1_000))
  $(testGroupGenerator)
