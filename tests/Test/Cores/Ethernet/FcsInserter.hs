{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NumericUnderscores #-}
module Test.Cores.Ethernet.FcsInserter where

-- base
import Prelude

-- clash-prelude
import qualified Clash.Prelude as C

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

-- util module
import Test.Cores.Ethernet.Util

-- data module
import qualified Data.List as L
import Data.Maybe

-- crc module
import Clash.Cores.Crc
import Clash.Cores.Ethernet.PacketStream
import Clash.Cores.Crc.Catalog
import Data.Proxy
import Data.Coerce
import Clash.Cores.Ethernet.FcsInserter
import Clash.Explicit.Prelude (listToVecTH)

genVec :: (C.KnownNat n, 1 C.<= n) => Gen a -> Gen (C.Vec n a)
genVec gen = sequence (C.repeat gen)

model :: [PacketStreamM2S 4 ()] -> [PacketStreamM2S 4 ()]
model fragments = insertCrc =<< chunkByPacket  fragments


packetToCrcInp
    :: [PacketStreamM2S 4 ()] -> [C.BitVector 8]
packetToCrcInp packet = C.head . _data <$> (chopPacket =<< packet)


insertCrc :: [PacketStreamM2S 4 ()] -> [PacketStreamM2S 4 ()]
insertCrc packet = L.init packet ++ [filled, trailing]
  where
    lastFragment = last packet
    softwareCrc = mkSoftwareCrc (Proxy @Crc32_ethernet) C.d8
    crcBytes = C.v2bv <$> (C.toList . C.reverse . C.unconcat C.d8 . C.bv2v . digest $ L.foldl' feed softwareCrc $ packetToCrcInp packet)
    lastValid = fromEnum . fromJust . _last $ lastFragment
    lastData = C.toList . _data $ lastFragment
    (filledData, trailingData) = splitAt 4 $ take (lastValid + 1) $ lastData L.++ crcBytes

    filled = lastFragment {
        _data = foldl (C.<<+) (0 C.:> 0 C.:> 0 C.:> 0 C.:> C.Nil) filledData
      , _last = Nothing
    }
    trailing = PacketStreamM2S {
        _data = foldl (C.<<+) (0 C.:> 0 C.:> 0 C.:> 0 C.:> C.Nil) trailingData
      , _last = _last lastFragment
      , _meta = ()
      , _abort = False
    }


-- | Test the fcsinserter
fcsinserterTest 
  :: HardwareCrc Crc32_ethernet 8 4 => Property
fcsinserterTest =
  propWithModelSingleDomain
    @C.System
    defExpectOptions
    (Gen.list (Range.linear 0 100) genPackets)                  -- Input packets
    (C.exposeClockResetEnable model)                            -- Desired behaviour of FcsInserter
    (C.exposeClockResetEnable ckt)               -- Implementation of FcsInserter
    (===)                                                       -- Property to test
  where
    ckt 
      :: forall (dom :: C.Domain)
      .  C.KnownDomain dom
      => C.HiddenClockResetEnable dom
      => HardwareCrc Crc32_ethernet 8 4
      => Circuit
        (PacketStream dom 4 ())
        (PacketStream dom 4 ())
    ckt = fcsInserterC

    -- This generates the packets
    genPackets =
      PacketStreamM2S <$>
      genVec Gen.enumBounded <*>
      Gen.maybe Gen.enumBounded <*>
      Gen.enumBounded <*>
      Gen.enumBounded

prop_fcsinserter_d4 
  :: HardwareCrc Crc32_ethernet 8 4 => Property
prop_fcsinserter_d4 = fcsinserterTest 

tests :: HardwareCrc Crc32_ethernet 8 4 => TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1_000))
  $(testGroupGenerator)
