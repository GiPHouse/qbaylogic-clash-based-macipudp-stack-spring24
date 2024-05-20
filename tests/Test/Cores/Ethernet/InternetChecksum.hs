{-# language FlexibleContexts #-}
{-# language NumericUnderscores #-}
{-# language RecordWildCards #-}

module Test.Cores.Ethernet.InternetChecksum where

-- base
import Data.Maybe
import Numeric ( showHex )
import Prelude

-- clash-prelude
import Clash.Prelude ( type (<=) )
import Clash.Prelude qualified as C

-- hedgehog
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

-- tasty
import Test.Tasty
import Test.Tasty.Hedgehog ( HedgehogTestLimit(HedgehogTestLimit) )
import Test.Tasty.Hedgehog.Extra ( testProperty )
import Test.Tasty.TH ( testGroupGenerator )

-- clash-cores
import Clash.Cores.Ethernet.InternetChecksum
import Data.Proxy

genVec :: (C.KnownNat n, 1 <= n) => Gen a -> Gen (C.Vec n a)
genVec gen = sequence (C.repeat gen)

genWord :: Gen (C.BitVector 16)
genWord = C.pack <$> genVec Gen.bool

genWordVec ::  (C.KnownNat n, 1 <= n) => Gen (C.Vec n (C.BitVector 16))
genWordVec = genVec genWord

-- functions used to print the intermediate state for debugging
showAsHex :: [C.BitVector 16] -> [String]
showAsHex = fmap (showSToString . Numeric.showHex . toInteger)
  where
    showSToString showS = showS ""

showComplementAsHex :: [C.BitVector 16] -> [String]
showComplementAsHex = showAsHex . fmap C.complement

flipBit :: Int -> Int ->  [Maybe (C.BitVector 16, Bool)] -> [Maybe (C.BitVector 16, Bool)]
flipBit listIndex bitIndex bitList = replaceAtIndex listIndex newWord bitList
  where
    replaceAtIndex :: Int -> a -> [a] -> [a]
    replaceAtIndex n item ls = a ++ (item:b) where (a, _ : b) = splitAt n ls

    newWord =fb (bitList !! listIndex)

    fb Nothing = Nothing
    fb (Just (word, flag)) = Just (C.complementBit word bitIndex, flag)

checkZeroAfterReset :: Int -> [Maybe (a, Bool)] -> [C.BitVector 16] -> Bool
checkZeroAfterReset _ [] _ = True
checkZeroAfterReset _ _ [] = False
checkZeroAfterReset d (Just (_, True):xs) yl@(_:ys) = checkZeroAfterDelay d yl && checkZeroAfterReset d xs ys
  where
    checkZeroAfterDelay :: Int -> [C.BitVector 16] -> Bool
    checkZeroAfterDelay _ [] = False
    checkZeroAfterDelay 0 (z:_) = z == 0x0
    checkZeroAfterDelay r (_:zs) = checkZeroAfterDelay (r-1) zs
checkZeroAfterReset d (_:xs) (_:ys) = checkZeroAfterReset d xs ys


-- Checks whether the checksum succeeds
prop_checksum_succeed :: Property
prop_checksum_succeed =
  property $ do
    let genInputList range = Gen.list range (Gen.maybe $ (,) <$> genWord <*> pure False)

    input <- forAll $ genInputList (Range.linear 1 100)
    let size = length input

    let checkSum = C.complement $ last $ take (size + 1) $ C.simulate @C.System internetChecksum input
        input' = input ++ [Just (checkSum, False)]
        checkSum' = last $ take (size + 2) $ C.simulate @C.System internetChecksum input'

    checkSum' === 0xFFFF

-- | Flips a random bit and checks whether the checksum actually fails
prop_checksum_fail :: Property
prop_checksum_fail =
  property $ do
    let genInputList range = Gen.list range (Just <$> ((,) <$> genWord <*> pure False))

    input <- forAll $ genInputList (Range.linear 1 100)
    let size = length input

    randomIndex <- forAll $ Gen.int (Range.linear 0 (size - 1))
    randomBitIndex <- forAll $ Gen.int (Range.linear 0 (16 - 1))

    let checkSum = C.complement $ last $ take (size + 1) $ C.simulate @C.System internetChecksum input
        input' = flipBit randomIndex randomBitIndex $ input ++ [Just (checkSum, False)]
        checkSum' = last $ take (size + 2) $ C.simulate @C.System internetChecksum input'

    checkSum' /== 0xFFFF

-- | testing the example from wikipedia: https://en.wikipedia.org/wiki/Internet_checksum
prop_checksum_specific_values :: Property
prop_checksum_specific_values =
  property $ do
    let input = Just . (, False) <$> [0x4500, 0x0073, 0x0000, 0x4000, 0x4011, 0x0000, 0xc0a8, 0x0001, 0xc0a8, 0x00c7]
        size = length input
        result = take (size + 1) $ C.simulate @C.System internetChecksum input
        checkSum = last result

    footnoteShow $ showAsHex result
    C.complement checkSum === 0xb861

-- | testing whether the value returns to 0 after a reset
prop_checksum_reset :: Property
prop_checksum_reset =
  property $ do
    let genInputList = Gen.list (Range.linear 1 100) (Gen.maybe $ (,) <$> genWord <*> Gen.bool)

    input <- forAll genInputList
    let size = length input
        result = take (size + 1) $ C.simulate @C.System internetChecksum input

    assert $ checkZeroAfterReset 1 input result

-- | testing the example from wikipedia: https://en.wikipedia.org/wiki/Internet_checksum
prop_checksum_reduce_specific_values :: Property
prop_checksum_reduce_specific_values =
  property $ do
    let input = Just . (, False) <$> [
          0x4500 C.:> 0x0073 C.:> 0x0000 C.:> C.Nil,
          0x4000 C.:> 0x4011 C.:> 0xc0a8 C.:> C.Nil,
          0x0001 C.:> 0xc0a8 C.:> 0x00c7 C.:> C.Nil
          ]
        size = length input
        result = take (size + 1) $ C.simulate @C.System reduceToInternetChecksum input
        checkSum = last result

    footnote $ "full output: " ++ show (showAsHex result)
    checkSum === 0x479e

prop_checksum_reduce_succeed :: Property
prop_checksum_reduce_succeed =
  property $ do
    let genInputList range = Gen.list range (Gen.maybe $ (,) <$> genWordVec @5 <*> pure False)

    input <- forAll $ genInputList (Range.linear 1 100)
    let size = length input

    let result = C.simulate @C.System reduceToInternetChecksum input
        checkSum = C.complement $ last $ take (size + 1) result
        input' = input ++ [Just (checkSum C.:> 0x0 C.:> 0x0 C.:> 0x0 C.:> 0x0 C.:> C.Nil, False)]
        checkSum' = last $ take (size + 2) $ C.simulate @C.System reduceToInternetChecksum input'

    checkSum' === 0xFFFF

prop_checksum_reduce_reset :: Property
prop_checksum_reduce_reset =
  property $ do
    let genInputList = Gen.list (Range.linear 1 100) (Gen.maybe $ (,) <$> genWordVec @5 <*> Gen.bool)

    input <- forAll genInputList
    let size = length input
        result = take (size + 1) $ C.simulate @C.System reduceToInternetChecksum input

    assert $ checkZeroAfterReset 1 input result

-- | testing the example from wikipedia: https://en.wikipedia.org/wiki/Internet_checksum
prop_checksum_pipeline_specific_values :: Property
prop_checksum_pipeline_specific_values =
  property $ do
    let input = Just . (,False)  <$> [
          0x4500 C.:> 0x0073 C.:> 0x0000 C.:> C.Nil,
          0x4000 C.:> 0x4011 C.:> 0xc0a8 C.:> C.Nil,
          0x0001 C.:> 0xc0a8 C.:> 0x00c7 C.:> C.Nil
          ]
        delay = fromInteger $ C.natVal (Proxy :: Proxy (InternetChecksumLatency 4)) + 1
        size = length input
        result = take (size + delay) $ C.simulate @C.System pipelinedInternetChecksum (input ++ replicate delay Nothing)
        checkSum = last result

    footnote $ "full output: " ++ show (showAsHex result)
    checkSum === 0x479e

prop_checksum_pipeline_succeed :: Property
prop_checksum_pipeline_succeed =
  property $ do
    let genInputList range = Gen.list range (Gen.maybe $ (,) <$> genWordVec @5 <*> pure False)
        delay = fromInteger $ C.natVal (Proxy :: Proxy (InternetChecksumLatency 5))

    input <- forAll $ genInputList (Range.linear 1 100)
    let size = length input

    let result = C.simulate @C.System pipelinedInternetChecksum (input ++ replicate delay Nothing)
        checkSum = C.complement $ last $ take (size + delay) result
        input' = input ++ [Just (checkSum C.:> 0x0 C.:> 0x0 C.:> 0x0 C.:> 0x0 C.:> C.Nil, False)] ++ replicate delay Nothing
        checkSum' = last $ take (size + delay + 1) $ C.simulate @C.System pipelinedInternetChecksum input'

    footnoteShow $ showAsHex $ take (size + delay) result

    checkSum' === 0xFFFF

prop_checksum_pipeline_reset :: Property
prop_checksum_pipeline_reset =
  property $ do
    let genInputList = Gen.list (Range.linear 1 100) (Gen.maybe $ (,) <$> genWordVec @7 <*> Gen.bool)
        delay = fromInteger $ C.natVal (Proxy :: Proxy (InternetChecksumLatency 7))
    input <- forAll genInputList
    let size = length input
        result = take (size + delay) $ C.simulate @C.System pipelinedInternetChecksum (input ++ replicate delay Nothing)

    footnoteShow $ showAsHex result

    assert $ checkZeroAfterReset delay input result

tests :: TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1_000))
  $(testGroupGenerator)
