{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Cores.Ethernet.Depacketizer where

-- base
import Prelude
import Data.Proxy
import Data.Maybe
import qualified Data.List as L

-- clash-prelude
import Clash.Prelude hiding (concatMap)
import qualified Clash.Prelude as C
import Clash.Prelude (type (<=))

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
import Clash.Cores.Ethernet.Depacketizer

import Test.Cores.Ethernet.Util

genVec :: (C.KnownNat n, 1 <= n) => Gen a -> Gen (C.Vec n a)
genVec gen = sequence (C.repeat gen)


model9 :: [PacketStreamM2S 9 ()] -> [PacketStreamM2S 9 EthernetHeader]
model9 fs = concatMap depacketize (chunkByPacket fs)
  where
    depacketize ::[PacketStreamM2S 9 ()] -> [PacketStreamM2S 9 EthernetHeader]
    depacketize (f1:f2:fs) = fragment : Prelude.map go fs
      where
        metaData = unpack $ pack $ _data f1 C.++ C.take d5 (_data f2)
        fragment = PacketStreamM2S {
          _data = C.replicate d9 0x00,
          _last = Nothing,
          _meta = metaData,
          _abort = False
        }
        go f = f {_meta = metaData}
    depacketize _ = []

prop_depacketizer :: Property
prop_depacketizer =
  propWithModelSingleDomain
    @C.System
    defExpectOptions
    (fmap addPacketWithLastSet (Gen.list (Range.linear 6 100) genPackets))
    (C.exposeClockResetEnable model9)
    (C.exposeClockResetEnable @C.System (macDepacketizerC d9))
    (===)
    where
  -- This is used to generate
        genPackets =
            PacketStreamM2S <$>
            (genVec Gen.enumBounded) <*>
            (Gen.maybe Gen.enumBounded) <*>
            Gen.enumBounded <*>
            Gen.enumBounded

tests :: TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1000))
  $(testGroupGenerator)
