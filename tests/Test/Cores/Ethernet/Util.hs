{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module Test.Cores.Ethernet.Util where

-- prelude
import Prelude

-- list
import qualified Data.List as L

-- maybe
import qualified Data.Maybe as M

-- clash-prelude
import qualified Clash.Prelude as C

-- ethernet modules
import Clash.Cores.Ethernet.PacketStream


chunkBy :: (a -> Bool) -> [a] -> [[a]]
chunkBy _ [] = []
chunkBy predicate list = L.filter (not . null) $ chunkByHelper predicate list []

-- Helper function to accumulate chunks
chunkByHelper :: (a -> Bool) -> [a] -> [a] -> [[a]]
chunkByHelper _ [] acc = [L.reverse acc]
chunkByHelper predicate (x : xs) acc
  | predicate x = L.reverse (x : acc) : chunkByHelper predicate xs []
  | otherwise = chunkByHelper predicate xs (x : acc)

chunkByPacket :: [PacketStreamM2S n meta] -> [[PacketStreamM2S n meta]]
chunkByPacket = chunkBy (M.isJust . _last)

smearAbort :: [PacketStreamM2S n meta] -> [PacketStreamM2S n meta]
smearAbort [] = []
smearAbort (x:xs) = L.reverse $ L.foldl' go [x] xs
  where
    go [] _ = []
    go l@(a:_) (PacketStreamM2S dat last' meta abort)
      = PacketStreamM2S dat last' meta (_abort a || abort):l

chopBy :: Int -> [a] -> [[a]]
chopBy _ [] = []
chopBy n xs = as : chopBy n bs where (as,bs) = splitAt n xs

chunkToPacket :: C.KnownNat n => [PacketStreamM2S 1 ()] -> PacketStreamM2S n ()
chunkToPacket l = PacketStreamM2S {
    _last = if M.isJust $ _last $ L.last l then M.Just (fromIntegral $ L.length l - 1) else Nothing
  , _abort = or $ fmap _abort l
  , _meta = ()
  , _data = L.foldr (C.+>>) (C.repeat 0) $ fmap (C.head . _data) l
}

singletonToPackets :: forall n. C.KnownNat n => [PacketStreamM2S n ()] -> [PacketStreamM2S 1 ()]
singletonToPackets [PacketStreamM2S {..}]
  | (C.natToInteger @n) <= 0 = []
  | otherwise = case _last of
    M.Just 0 -> [PacketStreamM2S {_last = M.Just 0, _abort = _abort, _meta = (), _data = C.take C.d1 _data}]
    M.Just i -> (PacketStreamM2S {_last = M.Nothing, _abort = _abort, _meta = (), _data = C.take C.d1 _data}) :
      singletonToPackets [PacketStreamM2S {_last = M.Just (i - 1), _abort = _abort, _meta = (), _data = C.init _data}]
    M.Nothing -> (PacketStreamM2S {_last = M.Nothing, _abort = _abort, _meta = (), _data = C.take C.d1 _data}) :
      singletonToPackets [PacketStreamM2S {_last = M.Nothing, _abort = _abort, _meta = (), _data = C.init _data}]
  
singletonToPackets _ = C.errorX "Called singletonToPackets on empty list"

