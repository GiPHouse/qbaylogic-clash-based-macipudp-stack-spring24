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

genVec :: (C.KnownNat n, 1 <= n) => Gen a -> Gen (C.Vec n a)
genVec gen = sequence (C.repeat gen)

genWord :: Gen (C.BitVector 16)
genWord = C.pack <$> genVec Gen.bool

-- functions used to print the intermediate state for debugging
showAsHex :: [C.BitVector 16] -> [String]
showAsHex = fmap (showSToString . Numeric.showHex . toInteger)
  where
    showSToString showS = showS ""

showComplementAsHex :: [C.BitVector 16] -> [String]
showComplementAsHex = showAsHex . fmap C.complement

-- Checks whether the checksum succeeds
prop_checksum_succeed :: Property
prop_checksum_succeed =
  property $ do
    let genInputList range = Gen.list range (Gen.maybe $ (,) <$> genWord <*> pure False)

    input <- forAll $ genInputList (Range.linear 1 100)
    let size = length input

    let checkSum = last $ take (size + 1) $ C.simulate @C.System internetChecksum input
        input' = input ++ [Just (checkSum, False)]
        checkSum' = last $ take (size + 2) $ C.simulate @C.System internetChecksum input'

    checkSum' === 0x0000

-- | Flips a random bit and checks whether the checksum actually fails
prop_checksum_fail :: Property
prop_checksum_fail =
  property $ do
    let genInputList range = Gen.list range (Just <$> ((,) <$> genWord <*> pure False))

    input <- forAll $ genInputList (Range.linear 1 100)
    let size = length input

    randomIndex <- forAll $ Gen.int (Range.linear 0 (size - 1))
    randomBitIndex <- forAll $ Gen.int (Range.linear 0 (16 - 1))

    let checkSum = last $ take (size + 1) $ C.simulate @C.System internetChecksum input
        input' = flipBit randomIndex randomBitIndex $ input ++ [Just (checkSum, False)]
        checkSum' = last $ take (size + 2) $ C.simulate @C.System internetChecksum input'

    checkSum' /== 0x0000
      where
        flipBit :: Int -> Int ->  [Maybe (C.BitVector 16, Bool)] -> [Maybe (C.BitVector 16, Bool)]
        flipBit listIndex bitIndex bitList = replaceAtIndex listIndex newWord bitList
          where
            replaceAtIndex :: Int -> a -> [a] -> [a]
            replaceAtIndex n item ls = a ++ (item:b) where (a, _ : b) = splitAt n ls

            newWord =fb (bitList !! listIndex)

            fb Nothing = Nothing
            fb (Just (word, flag)) = Just (C.complementBit word bitIndex, flag)

-- | testing the example from wikipedia: https://en.wikipedia.org/wiki/Internet_checksum
prop_checksum_specific_values :: Property
prop_checksum_specific_values =
  property $ do
    let input = Just . (, False) <$> [0x4500, 0x0073, 0x0000, 0x4000, 0x4011, 0x0000, 0xc0a8, 0x0001, 0xc0a8, 0x00c7]
        size = length input
        result = take (size + 1) $ C.simulate @C.System internetChecksum input
        checkSum = last result

    footnoteShow $ showComplementAsHex result
    checkSum === 0xb861

prop_checksum_reset :: Property
prop_checksum_reset =
  property $ do
    let genInputList = Gen.list (Range.linear 1 100) (Gen.maybe $ (,) <$> genWord <*> Gen.bool)

    input <- forAll genInputList
    let size = length input
        result = take size $ C.simulate @C.System internetChecksum input

    assert $ checkCurValueAfterReset False input result
      where
        checkCurValueAfterReset _ [] _ = True
        checkCurValueAfterReset _ _ [] = True
        checkCurValueAfterReset lastReset (Nothing:xs) (y:ys)           = (y == 0xFFFF || not lastReset) && checkCurValueAfterReset False xs ys
        checkCurValueAfterReset lastReset (Just (_, reset):xs) (y:ys)   = (y == 0xFFFF || not lastReset) && checkCurValueAfterReset reset xs ys 

tests :: TestTree
tests =
    localOption (mkTimeout 10_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1_000))
  $(testGroupGenerator)
