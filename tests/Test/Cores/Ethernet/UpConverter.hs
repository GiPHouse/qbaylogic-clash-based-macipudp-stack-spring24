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

-- Me
import Clash.Cores.Ethernet.UpConverter
import Clash.Cores.Ethernet.PacketStream

genVec :: (C.KnownNat n, 1 <= n) => Gen a -> Gen (C.Vec n a)
genVec gen = sequence (C.repeat gen)

-- | Test the upconverter stream instance
--   TODO: Use the fifo given by `DfConv`
prop_packetstream_sometest_id :: Property
prop_packetstream_sometest_id =
  propWithModelSingleDomain
    @C.System
    defExpectOptions
    (Gen.list (Range.linear 0 100) genPackets)
    (C.exposeClockResetEnable $ model)
    (C.exposeClockResetEnable @C.System (ckt @4))
    (\a b -> a === b)
  where
    ckt :: forall (dataWidth :: C.Nat) (dom :: C.Domain).
      C.HiddenClockResetEnable dom
      => 1 <= dataWidth
      => C.KnownNat dataWidth
      => Circuit (PacketStream dom 1 ()) (PacketStream dom dataWidth ())
    ckt = upConverterC

    model inpStream = PacketStreamM2S <$> outData <*> outLast <*> outMeta <*> outAbort where

      chunks = go [] [] 0 inpStream where
        go out _   _  []         = out
        go out acc sz (pkt:pkts) = case sz of
          4 -> go (out ++ [acc]) [] sz pkts
          _ -> case _last pkt of
            Nothing -> go out (acc ++ [pkt] ) (sz + 1) pkts
            Just _  -> go (out ++ [acc ++ [pkt]]) [] 0 pkts


      outMeta = replicate (length chunks)()
      outAbort= fmap (or. fmap _abort) chunks
      endsAbdruptly = fmap (M.isJust . _last . L.last) chunks
      outLast = fmap (\(ealy, ch) -> if ealy then Just (fromIntegral $ length ch ) else Nothing ) (zip endsAbdruptly chunks)

      padWithZeroes list = take 4 $ list ++ repeat 0
      chunkToVec v [] = v
      chunkToVec v (x:xs) = chunkToVec (v C.<<+ x) xs

      listOfDataOfTheChunks = fmap (fmap  _data) chunks
      listOfVecsOfTheChunks = fmap (fmap head) listOfDataOfTheChunks

      outData = fmap (chunkToVec ( 0 C.:> 0 C.:> 0 C.:> 0 C.:> C.Nil) . padWithZeroes . fmap _data) chunks




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
