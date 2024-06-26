{-# language FlexibleContexts #-}
{-# language NumericUnderscores #-}
{-# language RecordWildCards #-}

module Test.Cores.Ethernet.IP.InternetChecksum where

-- base
import Data.Maybe
import Data.Proxy
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

-- ethernet
import Clash.Cores.Ethernet.IP.InternetChecksum


uncurryS ::
  (C.Signal dom a -> C.Signal dom b -> C.Signal dom c)
  -> (C.Signal dom (a, b) -> C.Signal dom c)
uncurryS f a = f (C.fst <$> a) (C.snd <$> a)

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

flipBit :: Int -> Int -> [(Bool, Maybe (C.BitVector 16))] -> [(Bool, Maybe (C.BitVector 16))]
flipBit listIndex bitIndex bitList = replaceAtIndex listIndex newWord bitList
  where
    replaceAtIndex :: Int -> a -> [a] -> [a]
    replaceAtIndex n item ls = a ++ (item:b) where (a, _ : b) = splitAt n ls

    newWord = fb <$> (bitList !! listIndex)

    fb Nothing = Nothing
    fb (Just word) = Just (C.complementBit word bitIndex)

checkZeroAfterReset :: Int -> [(Bool, Maybe a)] -> [C.BitVector 16] -> Bool
checkZeroAfterReset _ [] _ = True
checkZeroAfterReset _ _ [] = False
checkZeroAfterReset d ((True, _):xs) yl@(_:ys) =
  checkZeroAfterDelay d yl && checkZeroAfterReset d xs ys
  where
    checkZeroAfterDelay :: Int -> [C.BitVector 16] -> Bool
    checkZeroAfterDelay _ [] = False
    checkZeroAfterDelay 0 (z:_) = z == 0x0
    checkZeroAfterDelay r (_:zs) = checkZeroAfterDelay (r-1) zs
checkZeroAfterReset d (_:xs) (_:ys) = checkZeroAfterReset d xs ys

extendInput :: Int -> [(Bool, Maybe x)] -> [(Bool, Maybe x)]
extendInput delay input = input ++ replicate delay (False, Nothing)

-- | Pure implementation of the RFC1079 internet checksum. Takes complement of
-- final outcome, unlike some components!
pureInternetChecksum :: Foldable t => t (C.BitVector 16) -> C.BitVector 16
pureInternetChecksum = C.complement . fromInteger . foldr (pureOnesComplementAdd . toInteger) 0

-- | Pure 16-bit one's complement sum for integers. Assumes that @a@ can store
-- large enough integers. Use something like `Int`, not `BitVector 16`.
pureOnesComplementAdd :: Integral a => a -> a -> a
pureOnesComplementAdd a b = (a + b) `mod` 65_536 + (a + b) `div` 65_536

-- Tests the one's complement sum
prop_onescomplementadd :: Property
prop_onescomplementadd = property $ do
  a <- forAll $ Gen.int (Range.linear 0 65_536)
  b <- forAll $ Gen.int (Range.linear 0 65_536)
  let c = pureOnesComplementAdd a b
  onesComplementAdd (C.fromIntegral a) (C.fromIntegral b) === C.fromIntegral c

-- Checks whether the checksum succeeds
prop_checksum_succeed :: Property
prop_checksum_succeed =
  property $ do
    let genInputList range = Gen.list range $ (,) False <$> Gen.maybe genWord

    input <- forAll $ genInputList (Range.linear 1 100)
    let size = length input

    let checkSum = C.complement $ last $ take (size + 1) $
                    C.simulate @C.System (uncurryS internetChecksum) input
        input' = input ++ [(False, Just checkSum)]
        checkSum' = last $ take (size + 2) $
                      C.simulate @C.System (uncurryS internetChecksum) input'

    checkSum' === 0xFFFF

-- | Flips a random bit and checks whether the checksum actually fails
prop_checksum_fail :: Property
prop_checksum_fail =
  property $ do
    let genInputList range = Gen.list range $ (,) False <$> (Just <$> genWord)

    input <- forAll $ genInputList (Range.linear 1 100)
    let size = length input

    randomIndex <- forAll $ Gen.int (Range.linear 0 (size - 1))
    randomBitIndex <- forAll $ Gen.int (Range.linear 0 (16 - 1))

    let checkSum = C.complement $ last $ take (size + 1) $
                    C.simulate @C.System (uncurryS internetChecksum) input
        input' = flipBit randomIndex randomBitIndex $ input ++ [(False, Just checkSum)]
        checkSum' = last $ take (size + 2) $ C.simulate @C.System (uncurryS internetChecksum) input'

    checkSum' /== 0xFFFF

-- | testing the example from wikipedia: https://en.wikipedia.org/wiki/Internet_checksum
prop_checksum_specific_values :: Property
prop_checksum_specific_values =
  property $ do
    let input = (False,) . Just <$> [0x4500, 0x0073, 0x0000, 0x4000, 0x4011, 0x0000, 0xc0a8, 0x0001, 0xc0a8, 0x00c7]
        size = length input
        result = take (size + 1) $
          C.simulate @C.System (uncurryS internetChecksum) input
        checkSum = last result

    footnoteShow $ showAsHex result
    C.complement checkSum === 0xb861

-- | testing whether the value returns to 0 after a reset
prop_checksum_reset :: Property
prop_checksum_reset =
  property $ do
    let genInputList = Gen.list (Range.linear 1 100) ((,) <$> Gen.bool <*> Gen.maybe genWord)

    input <- forAll genInputList
    let size = length input
        result = take (size + 1) $
          C.simulate @C.System (uncurryS internetChecksum) input

    footnoteShow $ showAsHex result
    assert $ checkZeroAfterReset 1 input result

-- | testing the example from wikipedia: https://en.wikipedia.org/wiki/Internet_checksum
prop_checksum_reduce_specific_values :: Property
prop_checksum_reduce_specific_values =
  property $ do
    let input = (False,) . Just <$> [
          0x4500 C.:> 0x0073 C.:> 0x0000 C.:> C.Nil,
          0x4000 C.:> 0x4011 C.:> 0xc0a8 C.:> C.Nil,
          0x0001 C.:> 0xc0a8 C.:> 0x00c7 C.:> C.Nil
          ]
        size = length input
        result = take (size + 1) $
          C.simulate @C.System (uncurryS reduceToInternetChecksum) input
        checkSum = last result

    footnote $ "full output: " ++ show (showAsHex result)
    checkSum === 0x479e

prop_checksum_reduce_succeed :: Property
prop_checksum_reduce_succeed =
  property $ do
    let genInputList range = Gen.list range ((,) False <$> Gen.maybe (genWordVec @5))

    input <- forAll $ genInputList (Range.linear 1 100)
    let size = length input

    let result = C.simulate @C.System (uncurryS reduceToInternetChecksum) input
        checkSum = C.complement $ last $ take (size + 1) result
        input' = input ++ [(False, Just (checkSum C.:> 0x0 C.:> 0x0 C.:> 0x0 C.:> 0x0 C.:> C.Nil))]
        checkSum' = last $ take (size + 2) $
          C.simulate @C.System (uncurryS reduceToInternetChecksum) input'

    checkSum' === 0xFFFF

prop_checksum_reduce_reset :: Property
prop_checksum_reduce_reset =
  property $ do
    let genInputList = Gen.list (Range.linear 1 100) ((,) <$> Gen.bool <*>  Gen.maybe (genWordVec @5))

    input <- forAll genInputList
    let size = length input
        result = take (size + 1) $ C.simulate @C.System (uncurryS reduceToInternetChecksum) input

    assert $ checkZeroAfterReset 1 input result

-- | testing the example from wikipedia: https://en.wikipedia.org/wiki/Internet_checksum
prop_checksum_pipeline_specific_values :: Property
prop_checksum_pipeline_specific_values =
  property $ do
    let input = (False,) . Just <$> [
          0x4500 C.:> 0x0073 C.:> 0x0000 C.:> C.Nil,
          0x4000 C.:> 0x4011 C.:> 0xc0a8 C.:> C.Nil,
          0x0001 C.:> 0xc0a8 C.:> 0x00c7 C.:> C.Nil
          ]
        delay = fromInteger $ C.natVal (Proxy :: Proxy (InternetChecksumLatency 4)) + 1
        size = length input
        result = take (size + delay) $
          C.simulate @C.System (uncurryS pipelinedInternetChecksum) (extendInput delay input)
        checkSum = last result

    footnote $ "full output: " ++ show (showAsHex result)
    checkSum === 0x479e

prop_checksum_pipeline_succeed :: Property
prop_checksum_pipeline_succeed =
  property $ do
    let genInputList range = Gen.list range ((False,) <$> Gen.maybe (genWordVec @5))
        delay = fromInteger $ C.natVal (Proxy :: Proxy (InternetChecksumLatency 5))

    input <- forAll $ genInputList (Range.linear 1 100)
    let size = length input

    let result = C.simulate @C.System (uncurryS pipelinedInternetChecksum) (extendInput delay input)
        checkSum = C.complement $ last $ take (size + delay) result
        input' = input
          ++ [(False, Just (checkSum C.:> 0x0 C.:> 0x0 C.:> 0x0 C.:> 0x0 C.:> C.Nil))]
          ++ extendInput delay input
        checkSum' = last $ take (size + delay + 1) $
          C.simulate @C.System (uncurryS pipelinedInternetChecksum) input'

    footnoteShow $ showAsHex $ take (size + delay) result

    checkSum' === 0xFFFF

prop_checksum_pipeline_reset :: Property
prop_checksum_pipeline_reset =
  property $ do
    let genInputList = Gen.list (Range.linear 1 100) ((,) <$> Gen.bool <*> Gen.maybe (genWordVec @7))
        delay = fromInteger $ C.natVal (Proxy :: Proxy (InternetChecksumLatency 7))
    input <- forAll genInputList
    let size = length input
        result = take (size + delay) $
          C.simulate @C.System (uncurryS pipelinedInternetChecksum)
          (input ++ extendInput delay input)

    footnoteShow $ showAsHex result

    assert $ checkZeroAfterReset delay input result

tests :: TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1_000))
  $(testGroupGenerator)
