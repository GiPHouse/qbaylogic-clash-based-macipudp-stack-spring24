{-# language FlexibleContexts #-}
{-# language RecordWildCards #-}
module Test.Cores.Ethernet.Util where

-- prelude
import Prelude

-- list
import Data.List qualified as L

-- maybe
import Data.Maybe qualified as M

-- clash-prelude
import Clash.Prelude qualified as C

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

chopPacket :: forall n. 1 C.<= n => C.KnownNat n => PacketStreamM2S n () -> [PacketStreamM2S 1 ()]
chopPacket PacketStreamM2S {..} = packets where
  lasts = case _last of
    Nothing  -> repeat Nothing
    Just in' -> replicate (fromIntegral in') Nothing ++ [Just (0 :: C.Index 1) ]

  datas = case _last of
    Nothing -> C.toList _data
    Just in' -> take (fromIntegral in' + 1) $ C.toList _data

  packets = (\(idx,  dat) -> PacketStreamM2S (pure dat) idx () _abort) <$> zip lasts datas

fullPackets :: (C.KnownNat n) => [PacketStreamM2S n meta] -> [PacketStreamM2S n meta]
fullPackets [] = []
fullPackets fragments = let lastFragment = (last fragments) { _last = Just 0 }
                        in  init fragments ++ [lastFragment]

addPacketWithLastSet :: (C.KnownNat n) => [PacketStreamM2S n meta] -> [PacketStreamM2S n meta]
addPacketWithLastSet [] = error "empty list not supported"
addPacketWithLastSet fs = fs ++ [(head fs) {_last = Just 0}]