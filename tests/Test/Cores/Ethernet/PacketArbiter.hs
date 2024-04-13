{-# language FlexibleContexts #-}
{-# language NumericUnderscores #-}
{-# language RecordWildCards #-}

module Test.Cores.Ethernet.PacketArbiter where

-- base
import Data.Maybe
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
import Clash.Cores.Ethernet.PacketArbiter
import Clash.Cores.Ethernet.PacketStream
import Test.Cores.Ethernet.Util

genVec :: (C.KnownNat n, 1 <= n) => Gen a -> Gen (C.Vec n a)
genVec gen = sequence (C.repeat gen)

prop_packetarbiter_roundrobin :: Property
prop_packetarbiter_roundrobin = makePropPacketArbiter C.d5 C.d2 RoundRobin

prop_packetarbiter_parallel :: Property
prop_packetarbiter_parallel = makePropPacketArbiter C.d5 C.d2 Clash.Cores.Ethernet.PacketArbiter.Parallel

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

    partitionPackets
      :: [PacketStreamM2S n (C.Index p)]
      -> C.Vec p [PacketStreamM2S n (C.Index p)]
    partitionPackets = snd . go
      where
        go [] = (maxBound, C.repeat [])
        go (x:xs) = (idx, C.replace idx (x:(v C.!! idx)) v)
          where
            (idx', v) = go xs
            idx = if isJust (_last x) then _meta x else idx'

tests :: TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1_000))
  $(testGroupGenerator)
