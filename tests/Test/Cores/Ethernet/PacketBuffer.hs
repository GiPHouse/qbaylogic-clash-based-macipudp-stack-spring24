{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Cores.Ethernet.PacketStream where

-- base
import Prelude
import Data.Proxy
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
import qualified Protocols.DfConv as DfConv

-- Me
import Clash.Cores.Ethernet.PacketStream
import Clash.Cores.Ethernet.PacketBuffer
import Protocols.Internal ( CSignal(..))

genVec :: (C.KnownNat n, 1 <= n) => Gen a -> Gen (C.Vec n a)
genVec gen = sequence (C.repeat gen)

-- | Test the packetBuffer fifo funtion
-- prop_packetBuffer_fifo_function :: Property
-- prop_packetBuffer_fifo_function =
--   idWithModelSingleDomain
--     @C.System
--     defExpectOptions
--     (Gen.list (Range.linear 0 100) genPackets)
--     (C.exposeClockResetEnable model)
--     (C.exposeClockResetEnable @C.System ckt)
--  where
--   ckt :: (C.HiddenClockResetEnable dom) =>
--     Circuit
--       (CSignal dom (Maybe (PacketStreamM2S 1 Int)))
--       (PacketStream dom 1 Int)
--   ckt = packetBufferC (SNat @8)

--   -- This is used to generate
--   genPackets =
--     Gen.maybe $
--     PacketStreamM2S <$>
--     genVec Gen.enumBounded <*>
--     Gen.maybe Gen.enumBounded <*>
--     Gen.enumBounded <*>
--     Gen.enumBounded

--   model = 
--     undefined

-- tests :: TestTree
-- tests =
--     localOption (mkTimeout 12_000_000 {- 12 seconds -})
--   $ localOption (HedgehogTestLimit (Just 1000))
--   $(testGroupGenerator)
