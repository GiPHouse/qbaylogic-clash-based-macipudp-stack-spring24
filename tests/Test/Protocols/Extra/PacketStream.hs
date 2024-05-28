{-# language FlexibleContexts #-}
{-# language NumericUnderscores #-}
{-# language RecordWildCards #-}

module Test.Protocols.Extra.PacketStream
  ( tests
  , chunkBy
  , chunkByPacket
  , smearAbort
  , chopBy
  , chunkToPacket
  , chopPacket
  , fullPackets
  , dropAbortedPackets
  , downConvert
  , upConvert
  , cleanPackets
  , makeValid
  , genValidPacket
  , genValidPackets
  ) where

-- base
import Data.List qualified as L
import Data.Maybe qualified as M
import Data.Proxy
import Prelude

-- clash
import Clash.Prelude ( type (<=) )
import Clash.Prelude qualified as C
import Clash.Sized.Vector qualified as Vec

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
import Protocols
import Protocols.DfConv qualified as DfConv
import Protocols.Extra.PacketStream
import Protocols.Hedgehog


genVec :: (C.KnownNat n, 1 <= n) => Gen a -> Gen (C.Vec n a)
genVec gen = sequence (C.repeat gen)

-- | Test the packet stream instance
--   TODO: Use the fifo given by `DfConv`
prop_packetstream_fifo_id :: Property
prop_packetstream_fifo_id =
  propWithModelSingleDomain
    @C.System
    defExpectOptions
    (Gen.list (Range.linear 0 100) genPackets)
    (C.exposeClockResetEnable id)
    (C.exposeClockResetEnable @C.System ckt)
    (\a b -> a === b)
 where
  ckt :: (C.HiddenClockResetEnable dom) =>
    Circuit
      (PacketStream dom 1 Int)
      (PacketStream dom 1 Int)
  ckt = DfConv.fifo Proxy Proxy (C.SNat @10)

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

-- | Partition a list based on given function
chunkBy :: (a -> Bool) -> [a] -> [[a]]
chunkBy _ [] = []
chunkBy predicate list = L.filter (not . null) $ chunkByHelper predicate list []

-- Helper function to accumulate chunks
chunkByHelper :: (a -> Bool) -> [a] -> [a] -> [[a]]
chunkByHelper _ [] acc = [L.reverse acc]
chunkByHelper predicate (x : xs) acc
  | predicate x = L.reverse (x : acc) : chunkByHelper predicate xs []
  | otherwise = chunkByHelper predicate xs (x : acc)

-- | Partition a list of PacketStreams into complete packets
chunkByPacket :: [PacketStreamM2S n meta] -> [[PacketStreamM2S n meta]]
chunkByPacket = chunkBy (M.isJust . _last)

-- | Smear abort over the rest of a list of packets
smearAbort :: [PacketStreamM2S n meta] -> [PacketStreamM2S n meta]
smearAbort [] = []
smearAbort (x:xs) = L.reverse $ L.foldl' go [x] xs
  where
    go [] _ = []
    go l@(a:_) (PacketStreamM2S dat last' meta abort)
      = PacketStreamM2S dat last' meta (_abort a || abort):l

-- | Partition a list into groups of given size
chopBy :: Int -> [a] -> [[a]]
chopBy _ [] = []
chopBy n xs = as : chopBy n bs where (as,bs) = splitAt n xs

-- | Merge a list of PacketStream 1 into a PacketStream n
chunkToPacket :: C.KnownNat n => [PacketStreamM2S 1 meta] -> PacketStreamM2S n meta
chunkToPacket l = PacketStreamM2S {
    _last = if M.isJust $ _last $ L.last l then M.Just (fromIntegral $ L.length l - 1) else Nothing
  , _abort = or $ fmap _abort l
  , _meta = _meta $ L.head l
  , _data = L.foldr (C.+>>) (C.repeat 0) $ fmap (C.head . _data) l
}

-- | Split a PacketStream n into a list of PacketStream 1
chopPacket :: forall n meta. 1 C.<= n => C.KnownNat n => PacketStreamM2S n meta -> [PacketStreamM2S 1 meta]
chopPacket PacketStreamM2S {..} = packets where
  lasts = case _last of
    Nothing  -> repeat Nothing
    Just in' -> replicate (fromIntegral in') Nothing ++ [Just (0 :: C.Index 1) ]

  datas = case _last of
    Nothing -> C.toList _data
    Just in' -> take (fromIntegral in' + 1) $ C.toList _data

  packets = (\(idx,  dat) -> PacketStreamM2S (pure dat) idx _meta _abort) <$> zip lasts datas

-- | Set the _last of the last element of a list of PacketStreams to 0
fullPackets :: (C.KnownNat n) => [PacketStreamM2S n meta] -> [PacketStreamM2S n meta]
fullPackets [] = []
fullPackets fragments = let lastFragment = (last fragments) { _last = Just 0 }
                        in  init fragments ++ [lastFragment]

-- | Drops packets if one of the words in the packet has the abort flag set
dropAbortedPackets :: [PacketStreamM2S n meta] -> [PacketStreamM2S n meta]
dropAbortedPackets packets = concat $ filter (not . any _abort) (chunkByPacket packets)

-- | Split a list of PacketStream n into a list of PacketStream 1
downConvert :: forall n meta. 1 C.<= n => C.KnownNat n => [PacketStreamM2S n meta] -> [PacketStreamM2S 1 meta]
downConvert = concatMap chopPacket

-- | Merge a list of PacketStream 1 into a list of PacketStream n
upConvert :: forall n meta. 1 C.<= n => C.KnownNat n => [PacketStreamM2S 1 meta] -> [PacketStreamM2S n meta]
upConvert packets = chunkToPacket <$> chopBy (C.natToNum @n) packets

-- | Set all invalid bytes to null-bytes
cleanPackets :: forall n meta. 1 C.<= n => C.KnownNat n => [PacketStreamM2S n meta] -> [PacketStreamM2S n meta]
cleanPackets = map cleanPacket
  where
    cleanPacket pkt@PacketStreamM2S{..} = case _last of
      Nothing -> pkt
      Just i  -> pkt {_data = M.fromJust $ Vec.fromList datas}
        where
          datas = take (1 + fromIntegral i) (C.toList _data) ++ replicate ((C.natToNum @n) - 1 - fromIntegral i) 0

-- | Make an existing list of packets valid, meaning all words in a packet share the same meta value, and the list always contain full packets
makeValid :: forall (dataWidth :: C.Nat) (metaType :: C.Type).
  C.KnownNat dataWidth
  => 1 C.<= dataWidth
  => [PacketStreamM2S dataWidth metaType]
  -> [PacketStreamM2S dataWidth metaType]
makeValid packets = fullPackets $ concatMap sameMeta $ chunkByPacket packets
  where
      sameMeta :: [PacketStreamM2S dataWidth metaType] -> [PacketStreamM2S dataWidth metaType]
      sameMeta [] = []
      sameMeta list@(x:_) = fullPackets $ fmap (\pkt -> pkt { _meta = _meta x }) list

-- | Generates a single valid packet using the given generator,
--   the meta value of the first word will be that of all words, and only the last value in the packet will have Last = Just ..
genValidPacket :: forall (dataWidth :: C.Nat) (metaType :: C.Type).
  C.KnownNat dataWidth
  => 1 C.<= dataWidth
  => Range Int
  -> Gen (PacketStreamM2S dataWidth metaType)
  -> Gen [PacketStreamM2S dataWidth metaType]
genValidPacket range gen = do
  genWords <- Gen.list range gen
  return $ makeValidPacket genWords
    where
      makeValidPacket :: [PacketStreamM2S dataWidth metaType] -> [PacketStreamM2S dataWidth metaType]
      makeValidPacket [] = []
      makeValidPacket list@(x:_) = fullPackets $ fmap (\pkt -> pkt { _meta = _meta x, _last = Nothing }) list

-- | Generates a list filled with valid packets, packets which have the same meta value for all words
genValidPackets :: forall (dataWidth :: C.Nat) (metaType :: C.Type).
  C.KnownNat dataWidth
  => 1 C.<= dataWidth
  => Range Int
  -- ^ Range specifying the amount of packets to generate
  -> Range Int
  -- ^ Range specifying the size of packets to generate
  -> Gen (PacketStreamM2S dataWidth metaType)
  -- ^ Generator for a single packet
  -> Gen [PacketStreamM2S dataWidth metaType]
genValidPackets range packetRange gen = concat <$> Gen.list range (genValidPacket packetRange gen)
