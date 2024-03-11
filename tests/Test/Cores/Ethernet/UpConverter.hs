{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Cores.Ethernet.UpConverter where

-- base
import Prelude

-- list
import qualified Data.List as L

-- maybe
import qualified Data.Maybe as M

-- clash-prelude
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

-- ethernet modules
import Clash.Cores.Ethernet.UpConverter
import Clash.Cores.Ethernet.PacketStream

genVec :: (C.KnownNat n, 1 <= n) => Gen a -> Gen (C.Vec n a)
genVec gen = sequence (C.repeat gen)

-- | Test the upconverter stream instance
prop_upconverter :: Property
prop_upconverter =
  propWithModelSingleDomain
    @C.System
    defExpectOptions
    (Gen.list (Range.linear 0 100) genPackets)    -- Input packets
    (C.exposeClockResetEnable model)              -- Desired behaviour of UpConverter
    (C.exposeClockResetEnable @C.System (ckt @4)) -- Implementation of UpConverter
    (===)                                         -- Property to test
  where
    ckt :: forall (dataWidth :: C.Nat) (dom :: C.Domain).
      C.HiddenClockResetEnable dom
      => 1 <= dataWidth
      => C.KnownNat dataWidth
      => Circuit (PacketStream dom 1 ()) (PacketStream dom dataWidth ())
    ckt = upConverterC

    model inpStream = PacketStreamM2S <$> outData <*> outLast <*> outMeta <*> outAbort where

      chunks = go [] [] 0 inpStream where
        go :: [[PacketStreamM2S 1 ()]] -- Global accumulator, will be given as output
           -> [PacketStreamM2S 1 ()]   -- Accumulator
           -> Integer                  -- Size of accumulator (redundant?)
           -> [PacketStreamM2S 1 ()]   -- Input stream
           -> [[PacketStreamM2S 1 ()]]
        go out _   _    []         = out                      -- No more PacketStream to handle, give output
        go out acc size (pkt:pkts) = case size of             -- We have some PacketStreams to handle
          3 -> go (out ++ [acc ++ [pkt]]) [] 0 pkts           -- Accumulator is full, move them to output
          _ -> case _last pkt of                              -- Accumulator is not full
            Nothing -> go out (acc ++ [pkt] ) (size + 1) pkts -- PacketStream is not the last, so add it to accumulator
            Just _  -> go (out ++ [acc ++ [pkt]]) [] 0 pkts   -- Last PacketStream of packet, move to output

      -- We don't need any metadeta
      outMeta = replicate (length chunks) ()
      -- If abort is set for one PacketStream of the chunk, it should be set over the whole chunk
      outAbort = fmap (or. fmap _abort) chunks
      -- The _last of each chunk is the _last of the last byte of the chunk
      endsAbruptly = fmap (M.isJust . _last . L.last) chunks
      outLast = fmap (\(hasLast, chunk) ->
        if hasLast then Just (fromIntegral $ length chunk) else Nothing ) (zip endsAbruptly chunks)
      -- Fill each chunk up to 4, then group each individual Vec 1 into a Vec 4
      padWithZeroes list = take 4 $ list ++ repeat 0
      chunkToVec v [] = v
      chunkToVec v (x:xs) = chunkToVec (v C.<<+ x) xs

      listOfDataOfTheChunks = fmap (fmap  _data) chunks
      listOfVecsOfTheChunks = fmap (fmap C.head) listOfDataOfTheChunks

      outData = fmap (chunkToVec ( 0 C.:> 0 C.:> 0 C.:> 0 C.:> C.Nil) . padWithZeroes) listOfVecsOfTheChunks
    -- This generates the packets
    genPackets =
      PacketStreamM2S <$>
      genVec Gen.enumBounded <*>
      Gen.maybe Gen.enumBounded <*>
      Gen.enumBounded <*>
      Gen.enumBounded

tests :: TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1_000))
  $(testGroupGenerator)
