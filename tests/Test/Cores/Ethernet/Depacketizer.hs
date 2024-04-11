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

-- | Shifts a PacketStream to the left by n bytes, and drops these bytes
shiftStreamL9 :: forall (n :: Nat) (meta :: Type) .
  (KnownNat n
  , 1 <= n
  , n <= 9)
  => SNat n
  -> [PacketStreamM2S 9 meta] -> [PacketStreamM2S 9 meta]
shiftStreamL9 n [] = error "unreachable"
shiftStreamL9 n (f:fs) = PacketStreamM2S {
  _data = oldData C.++ newData,
  _last = Prelude.undefined,
  _meta = Prelude.undefined,
  _abort = False
} : shiftStreamL9 n fs
  where
    oldData :: Vec (9 - n) (BitVector 8)
    oldData = C.take (subSNat d9 n) (_data f)
    newData :: Vec n (BitVector 8)
    newData = C.take n (_data (L.head fs))

model9 :: [PacketStreamM2S 9 ()] -> [PacketStreamM2S 9 EthernetHeader]
model9 fs = concatMap depacketize (chunkByPacket fs)
  where
    depacketize ::[PacketStreamM2S 9 ()] -> [PacketStreamM2S 9 EthernetHeader]
    depacketize (f1:f2:fs) = shiftStreamL9 d5 $ fragment : Prelude.map go fs
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
