{-# language FlexibleContexts #-}

module Test.Cores.Ethernet.Depacketizer
  (depacketizerModel) where

-- base
import Data.List qualified as L
import Data.Maybe
import Prelude

-- clash-prelude
import Clash.Prelude hiding ( concatMap )
import Clash.Prelude qualified as C

-- Me
import Clash.Cores.Ethernet.PacketStream

import Test.Cores.Ethernet.Util


-- | Shifts a PacketStream to the left by n bytes, drops these bytes and adjusts the byte enable at the end accordingly.
-- Assumes that the only fragment in the list that has _last set is the fragment at the end.
-- To achieve this, see `chopByPacket`.
shiftStreamL :: forall (dataWidth :: Nat) (n :: Nat) (meta :: Type) .
  ( KnownNat dataWidth
  , KnownNat n
  , 1 <= dataWidth
  , n <= dataWidth)
  => SNat n
  -> SNat dataWidth
  -> PacketStreamM2S dataWidth meta
  -> [PacketStreamM2S dataWidth meta]
  -> [PacketStreamM2S dataWidth meta]
shiftStreamL _ _ _ [] = []
shiftStreamL n dataWidth old [f] = if fromJust (_last f) >= snatToNum n
  then f1:[f2]
  else [f3]
    where
      i = fromJust (_last f)
      oldData = C.take (subSNat dataWidth n) (rotateLeftS (_data old) n)
      newData = C.take n (_data f)
      f1 = f {
        _data = oldData C.++ newData,
        _last = Nothing
      }
      f2 = f {
        _data = rotateLeftS (_data f) n,
        _last = Just (satAdd SatWrap i (snatToNum (subSNat dataWidth n)))
      }
      f3 = f1 {
        _last = Just (i + snatToNum (subSNat dataWidth n))
      }
shiftStreamL n dataWidth old (f:fs) = old {_data = oldData C.++ newData} : shiftStreamL n dataWidth f fs
  where
    oldData = C.take (subSNat dataWidth n :: SNat (dataWidth-n)) (rotateLeftS (_data old) n)
    newData = C.take n (_data f :: Vec (n + (dataWidth-n)) (BitVector 8)) :: Vec n (BitVector 8)

-- | Depacketizes `n` bytes and puts this in the `_meta` field of all fragments.
-- If the number of fragments in the input list is lower than the number we needed to depacketize `n` bytes,
-- then the output boolean is False.
depacketize :: forall (dataWidth :: Nat) (n :: Nat) (metaIn :: Type) (metaOut :: Type) .
  ( KnownNat dataWidth
  , KnownNat n
  , 1 <= dataWidth
  , Mod n dataWidth <= dataWidth
  , BitPack metaOut
  , BitSize metaOut ~ n * 8)
  => SNat dataWidth
  -> SNat n
  -> Vec n (BitVector 8)
  -> Index n
  -> [PacketStreamM2S dataWidth metaIn]
  -> (PacketStreamM2S dataWidth metaOut, [PacketStreamM2S dataWidth metaOut], Bool)
-- Could not depacketize, too little fragments. We signal this with a boolean.
depacketize _ _ _ _ [] = (Prelude.undefined, Prelude.undefined, False)
depacketize dataWidth n cache i (x:xs) = out
  where
    out = case compareSNat n dataWidth of
      SNatLE -> (old, rest, True)
        where
          old = x {_meta = metaData}
          rest = L.map (\f -> f  {_meta = metaData}) xs
          metaData = unpack $ pack (C.take n (_data x :: Vec (n + (dataWidth-n)) (BitVector 8)))
      SNatGT ->
        if satAdd SatZero i (snatToNum dataWidth) == 0
        then (old, rest, True)
        else depacketize dataWidth n newCache2 (i + snatToNum dataWidth) xs
          where
            metaData = unpack $ pack newCache1
            old = x {_meta = metaData}
            rest = L.map (\f -> f {_meta = metaData}) xs
            newCache1 :: Vec n (BitVector 8) = case compareSNat (modSNat n dataWidth) d0 of
              SNatLE -> newCache2
              SNatGT -> fst $ shiftInAtN cache (C.take (modSNat n dataWidth) (_data x :: Vec (Mod n dataWidth + (dataWidth - Mod n dataWidth)) (BitVector 8)))
            newCache2 = fst $ shiftInAtN cache (_data x)

-- | Model of the generic `depacketizerC`.
depacketizerModel :: forall (dataWidth :: Nat) (n :: Nat) (metaIn :: Type) (metaOut :: Type) .
  ( KnownNat dataWidth
  , KnownNat n
  , 1 <= dataWidth
  , 1 <= n
  , Mod n dataWidth <= dataWidth
  , BitPack metaOut
  , BitSize metaOut ~ n * 8)
  => SNat dataWidth
  -> SNat n
  -> [PacketStreamM2S dataWidth metaIn]
  -> [PacketStreamM2S dataWidth metaOut]
depacketizerModel dataWidth n ps = concatMap go (chunkByPacket ps)
  where
    go fs = case compareSNat n dataWidth of
      SNatLE -> if good then case compareSNat dataWidth n of
        -- dataWidth ~ n: no shifting necessary.
        SNatLE -> rest
        SNatGT -> shiftStreamL n dataWidth old rest else []
      SNatGT -> if good then case compareSNat (modSNat n dataWidth) d0 of
        -- n mod dataWidth ~ 0: no shifting necessary.
        SNatLE -> rest
        SNatGT -> shiftStreamL (modSNat n dataWidth) dataWidth old rest else []
      where
        (old :: PacketStreamM2S dataWidth metaOut, rest, good) = depacketize dataWidth n (C.replicate n 0xAB) 0 fs
