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
{-shiftStreamL :: forall (dataWidth :: Nat) (n :: Nat) (g :: Nat) (meta :: Type) .
  (KnownNat n
  , 1 <= n
  , n <= 8
  , 1 <= dataWidth
  , n + g ~ dataWidth)
  => SNat n
  -> SNat dataWidth
  -> PacketStreamM2S dataWidth meta -> [PacketStreamM2S dataWidth meta] -> [PacketStreamM2S dataWidth meta]

shiftStreamL _ _ _ [] = []
shiftStreamL n dataWidth old [f] = let i = fromJust (_last f) in
  if i >= snatToNum n -- if i > n then send 2 fragments else 1
  then x:[y]
  else [z]
    where

      i = fromJust (_last f)
      dataa :: Vec dataWidth (BitVector 8)
      dataa = oldData C.++ newData
      oldData :: Vec (dataWidth - n) (BitVector 8)
      oldData = C.take (subSNat dataWidth n) (rotateLeftS (_data old) n)
      newData :: Vec n (BitVector 8)
      newData = C.take n (_data f)
      temp = PacketStreamM2S {
        _data = dataa,
        _last = Nothing,
        _meta = _meta f,
        _abort = False
      }
      z = temp {_last = Just (i + 4)}--{_last = Just (satAdd SatWrap i (9 - (snatToNum n)))}
      x = temp
      newnewData = rotateLeftS (_data f) n
      y = temp {
        _data = newnewData,
        _last = Just (satAdd SatWrap i (snatToNum (subSNat dataWidth n)))
      }

shiftStreamL n dataWidth old (f:fs) = PacketStreamM2S {
  _data = dataa,
  _last = Nothing,
  _meta = _meta old,
  _abort = False
} : shiftStreamL n dataWidth f fs
  where
    dataa :: Vec dataWidth (BitVector 8)
    dataa = oldData C.++ newData
    oldData :: Vec (dataWidth - n) (BitVector 8)
    oldData = C.take (subSNat dataWidth n) (rotateLeftS (_data old) n)
    newData :: Vec n (BitVector 8)
    newData = C.take n (_data f)-}




-- | Shifts a PacketStream to the left by n bytes, and drops these bytes
shiftStreamL9 :: forall (n :: Nat) (g :: Nat) (meta :: Type) .
  (KnownNat n
  , 1 <= n
  , n <= 8
  , n + g ~ 9)
  => SNat n
  -> PacketStreamM2S 9 meta -> [PacketStreamM2S 9 meta] -> [PacketStreamM2S 9 meta]

shiftStreamL9 _ _ [] = []
shiftStreamL9 n old [f] = let i = fromJust (_last f) in
  if i >= snatToNum n -- if i > n then send 2 fragments else 1
  then x:[y]
  else [z]
    where

      i = fromJust (_last f)
      dataa :: Vec 9 (BitVector 8)
      dataa = oldData C.++ newData
      oldData :: Vec (9 - n) (BitVector 8)
      oldData = C.take (subSNat d9 n) (rotateLeftS (_data old) n)
      newData :: Vec n (BitVector 8)
      newData = C.take n (_data f)
      temp = PacketStreamM2S {
        _data = dataa,
        _last = Nothing,
        _meta = _meta f,
        _abort = False
      }
      z = temp {_last = Just (i + 4)}--{_last = Just (satAdd SatWrap i (9 - (snatToNum n)))}
      x = temp
      newnewData = rotateLeftS (_data f) n
      y = temp {
        _data = newnewData,
        --_last = Just (satAdd SatWrap i (snatToNum (subSNat d9 n :: SNat (9-n))))
        _last = Just (satAdd SatWrap i ((8 - snatToNum n) + 1))
      }

shiftStreamL9 n old (f:fs) = PacketStreamM2S {
  _data = dataa,
  _last = Nothing,
  _meta = _meta old,
  _abort = False
} : shiftStreamL9 n f fs
  where
    dataa :: Vec 9 (BitVector 8)
    dataa = oldData C.++ newData
    oldData :: Vec (9 - n) (BitVector 8)
    oldData = C.take (subSNat d9 n) (rotateLeftS (_data old) n)
    newData :: Vec n (BitVector 8)
    newData = C.take n (_data f)


depacketize ::[PacketStreamM2S 9 ()] -> (PacketStreamM2S 9 EthernetHeader, [PacketStreamM2S 9 EthernetHeader])
depacketize (f1:f2:fs) = (f2 {_meta = metaData}, Prelude.map go fs)
  where
    metaData = unpack $ pack $ _data f1 C.++ C.take d5 (_data f2)
    go f = f {_meta = metaData}
depacketize _ = error "expected a list with >= 2 elements"


model9 :: [PacketStreamM2S 9 ()] -> [PacketStreamM2S 9 EthernetHeader]
model9 bs = concatMap go (chunkByPacket bs)
  where
    go fs = if L.length fs < 2 then [] else shiftStreamL9 d5 old ds
      where
        (old, ds) = depacketize fs


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
            Gen.enum False False
            --fmap (const False) Gen.enumBounded

tests :: TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1000))
  $(testGroupGenerator)
