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

-- hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen

-- | Split a list into a list of lists, starting a new list whenever the predicate is true
chunkBy :: (a -> Bool) -> [a] -> [[a]]
chunkBy _ [] = []
chunkBy predicate list = L.filter (not . null) $ chunkByHelper predicate list []

-- Helper function to accumulate chunks
chunkByHelper :: (a -> Bool) -> [a] -> [a] -> [[a]]
chunkByHelper _ [] acc = [L.reverse acc]
chunkByHelper predicate (x : xs) acc
  | predicate x = L.reverse (x : acc) : chunkByHelper predicate xs []
  | otherwise = chunkByHelper predicate xs (x : acc)

-- | Splits a list of PacketStreamM2S into lists of bytes from the same packet
chunkByPacket :: [PacketStreamM2S n meta] -> [[PacketStreamM2S n meta]]
chunkByPacket = chunkBy (M.isJust . _last)

-- | Sets abort bit of all packets in list if one of the packets has it
smearAbort :: [PacketStreamM2S n meta] -> [PacketStreamM2S n meta]
smearAbort [] = []
smearAbort (x:xs) = L.reverse $ L.foldl' go [x] xs
  where
    go [] _ = []
    go l@(a:_) (PacketStreamM2S dat last' meta abort)
      = PacketStreamM2S dat last' meta (_abort a || abort):l

-- | Split a list every n elements
chopBy :: Int -> [a] -> [[a]]
chopBy _ [] = []
chopBy n xs = as : chopBy n bs where (as,bs) = splitAt n xs

-- | Merge a list of PacketStreamM2S into a single PacketStreamM2S with the same bytes
chunkToPacket :: C.KnownNat n => [PacketStreamM2S 1 ()] -> PacketStreamM2S n ()
chunkToPacket l = PacketStreamM2S {
    _last = if M.isJust $ _last $ L.last l then M.Just (fromIntegral $ L.length l - 1) else Nothing
  , _abort = or $ fmap _abort l
  , _meta = ()
  , _data = L.foldr (C.+>>) (C.repeat 0) $ fmap (C.head . _data) l
}

-- | Splits a PacketStreamM2S into a list of PacketStreamM2Ss containing a single byte
chopPacket :: forall n. 1 C.<= n => C.KnownNat n => PacketStreamM2S n () -> [PacketStreamM2S 1 ()]
chopPacket PacketStreamM2S {..} = packets where
  lasts = case _last of
    Nothing  -> repeat Nothing
    Just in' -> replicate (fromIntegral in') Nothing ++ [Just (0 :: C.Index 1) ]

  datas = case _last of
    Nothing -> C.toList _data
    Just in' -> take (fromIntegral in' + 1) $ C.toList _data

  packets = (\(idx,  dat) -> PacketStreamM2S (pure dat) idx () _abort) <$> zip lasts datas

-- | Generates packets with random data, random last bit and random meta data
genPacket
  :: (C.KnownNat n, 1 C.<= n)
  => Gen a
  -> Gen (PacketStreamM2S n a)
genPacket genMeta = PacketStreamM2S <$>
  genVec Gen.enumBounded <*>
  Gen.maybe Gen.enumBounded <*>
  genMeta <*>
  Gen.enumBounded

-- | Generates vectors
genVec :: (C.KnownNat n, 1 C.<= n) => Gen a -> Gen (C.Vec n a)
genVec gen = sequence (C.repeat gen)
