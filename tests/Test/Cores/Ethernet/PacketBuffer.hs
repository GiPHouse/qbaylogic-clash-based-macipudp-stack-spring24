{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Cores.Ethernet.PacketStream where

-- base
import Prelude
import Data.Proxy
import Data.Maybe

-- clash-prelude
import qualified Clash.Prelude as C
import Clash.Prelude hiding (undefined)

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
import qualified Protocols.DfConv as DfConv

-- Me
import Clash.Cores.Ethernet.PacketStream
import Clash.Cores.Ethernet.PacketBuffer

genVec :: (C.KnownNat n, 1 <= n) => Gen a -> Gen (C.Vec n a)
genVec gen = sequence (C.repeat gen)

prop_id_without_nothings :: Property
prop_id_without_nothings = 
    propWithModelSingleDomain
    @C.System
    defExpectOptions
    (Gen.list (Range.linear 0 100) genPackets)
    (C.exposeClockResetEnable id)
    (C.exposeClockResetEnable @C.System ckt)
    (===)
 where
  ckt :: (C.HiddenClockResetEnable dom) =>
    Circuit
      (PacketStream dom 1 Int)
      (PacketStream dom 1 Int)
  ckt = DfConv.fifo Proxy Proxy (C.SNat @10)

  -- This is used to generate
  genPackets =
    PacketStreamM2S <$>
    genVec Gen.enumBounded <*>
    Gen.maybe Gen.enumBounded <*>
    Gen.enumBounded <*>
    Gen.enumBounded

removeNothings :: forall (dataWidth :: Nat) (a ::PacketStreamM2S dataWidth ()). [Maybe a] -> [Maybe a]
removeNothings = filter isJust

prop_handle_overflow :: Property
prop_handle_overflow = undefined

tests :: TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1_000))
  $(testGroupGenerator)
