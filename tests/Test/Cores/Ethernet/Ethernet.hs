module Test.Cores.Ethernet.Ethernet where

import Prelude

import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.TH

import Hedgehog ( (===) )
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

prop_plusIsCommutative :: H.Property
prop_plusIsCommutative = H.property $ do
  a <- H.forAll (Gen.integral (Range.linear (minBound :: Int) maxBound))
  b <- H.forAll (Gen.integral (Range.linear (minBound :: Int) maxBound))
  a + b === b + a

tests :: TestTree
tests = $(testGroupGenerator)

main :: IO ()
main = defaultMain tests
