{-# language FlexibleContexts #-}
{-# language NumericUnderscores #-}
{-# language RecordWildCards #-}

module Test.Protocols.Extra.PacketStream.Routing where

-- base
import Data.List ( groupBy, sortOn )
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
import Protocols.Extra.PacketStream
import Protocols.Extra.PacketStream.Routing
import Protocols.Hedgehog

-- tests
import Test.Protocols.Extra.PacketStream ( chunkByPacket, fullPackets )


genVec :: (C.KnownNat n, 1 <= n) => Gen a -> Gen (C.Vec n a)
genVec gen = sequence (C.repeat gen)

-- | Tests the round-robin packet arbiter with one source; essentially an id test
prop_packetarbiter_roundrobin_id :: Property
prop_packetarbiter_roundrobin_id = makePropPacketArbiter C.d1 C.d2 RoundRobin

-- | Tests the parallel packet arbiter with one source; essentially an id test
prop_packetarbiter_parallel_id :: Property
prop_packetarbiter_parallel_id = makePropPacketArbiter C.d1 C.d2 Protocols.Extra.PacketStream.Routing.Parallel

-- Tests the round-robin arbiter with five sources
prop_packetarbiter_roundrobin :: Property
prop_packetarbiter_roundrobin = makePropPacketArbiter C.d5 C.d2 RoundRobin

-- Tests the parallel arbiter with five sources
prop_packetarbiter_parallel :: Property
prop_packetarbiter_parallel = makePropPacketArbiter C.d5 C.d2 Protocols.Extra.PacketStream.Routing.Parallel

-- | Tests a packet arbiter for any data width and number of sources. In particular,
-- tests that packets from all sources are sent out unmodified in the same order
-- they were in in the source streams.
makePropPacketArbiter
  :: forall p n
   . ( C.KnownNat p
     , 1 <= p
     , C.KnownNat n
     , 1 <= n
     )
  => C.SNat p
  -> C.SNat n
  -> ArbiterMode
  -> Property
makePropPacketArbiter _ _ mode = propWithModelSingleDomain
  @C.System
  defExpectOptions
  genSources
  (C.exposeClockResetEnable concat)
  (C.exposeClockResetEnable (packetArbiterC mode))
  (\xs ys -> partitionPackets xs === partitionPackets ys)
  where
    genSources = mapM (fmap fullPackets . Gen.list (Range.linear 0 100) . genPacket) (C.indicesI @p)
    -- TODO use util function from client review branch
    genPacket i =
      PacketStreamM2S <$>
      genVec @n Gen.enumBounded <*>
      Gen.maybe Gen.enumBounded <*>
      pure i <*>
      Gen.enumBounded

    partitionPackets packets = sortOn (_meta . head . head) $
      groupBy (\a b -> _meta a == _meta b) <$> chunkByPacket packets

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
