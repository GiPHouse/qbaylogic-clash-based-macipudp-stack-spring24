{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Cores.Ethernet.AsyncFIFO where

-- base
import Prelude

-- clash-prelude
import Clash.Prelude as C

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
import Clash.Cores.Ethernet.PacketStream
    ( PacketStream, PacketStreamM2S(PacketStreamM2S) )
import Clash.Cores.Ethernet.AsyncFIFO

genVec :: (KnownNat n, 1 C.<= n) => Gen a -> Gen (Vec n a)
genVec gen = sequence (C.repeat gen)

createDomain vSystem
  { vName="TestDom50"
  , vPeriod=20000
  , vActiveEdge=Rising
  , vResetKind=Asynchronous
  , vInitBehavior=Unknown
  , vResetPolarity=ActiveHigh
  }

createDomain vSystem
  { vName="TestDom125"
  , vPeriod=8000
  , vActiveEdge=Rising
  , vResetKind=Asynchronous
  , vInitBehavior=Unknown
  , vResetPolarity=ActiveHigh
  }

clk50 :: Clock TestDom50
clk50 = clockGen

clk125 :: Clock TestDom125
clk125 = clockGen

rst50 :: Reset TestDom50
rst50 = resetGen @TestDom50

rst125 :: Reset TestDom125
rst125 = resetGen @TestDom125

en50 :: Enable TestDom50
en50 = enableGen

en125 :: Enable TestDom125
en125 = enableGen

generateAsyncFifoIdProp :: forall (wDom :: Domain) (rDom :: Domain) .
  (KnownDomain wDom, KnownDomain rDom) =>
  Clock wDom
  -> Reset wDom
  -> Enable wDom
  -> Clock rDom
  -> Reset rDom
  -> Enable rDom
  -> Property
generateAsyncFifoIdProp wClk wRst wEn rClk rRst rEn =
  propWithModel
    defExpectOptions
    (Gen.list (Range.linear 0 100) genPackets)
    id
    ckt
    (===)
    where
      ckt :: (KnownDomain wDom, KnownDomain rDom) => Circuit
              (PacketStream wDom 1 Int)
              (PacketStream rDom 1 Int)
      ckt = asyncFifoC (C.SNat @64) wClk wRst wEn rClk rRst rEn
      -- This is used to generate
      genPackets =
          PacketStreamM2S <$>
          genVec Gen.enumBounded <*>
          Gen.maybe Gen.enumBounded <*>
          Gen.enumBounded <*>
          Gen.enumBounded

-- | Test the Async FIFO
prop_asyncfifo_writer_speed_equal_to_reader_id :: Property
prop_asyncfifo_writer_speed_equal_to_reader_id = generateAsyncFifoIdProp clk50 rst50 en50 clk50 rst50 en50

-- | Test the Async FIFO
prop_asyncfifo_writer_speed_slower_than_reader_id :: Property
prop_asyncfifo_writer_speed_slower_than_reader_id = generateAsyncFifoIdProp clk50 rst50 en50 clk125 rst125 en125

-- | Test the Async FIFO
prop_asyncfifo_writer_speed_faster_than_reader_id :: Property
prop_asyncfifo_writer_speed_faster_than_reader_id = generateAsyncFifoIdProp clk125 rst125 en125 clk50 rst50 en50

tests :: TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1_000))
  $(testGroupGenerator)
  
