{-# OPTIONS_GHC -Wno-orphans #-}
{-# language FlexibleContexts #-}
{-# language MultiParamTypeClasses #-}
{-# language NumericUnderscores #-}
{-# language RecordWildCards #-}
{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}

module Test.Cores.Ethernet.FcsInserter where

-- base
import Prelude

-- clash-prelude
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

-- clash-protocols
import Protocols
import Protocols.Hedgehog

-- util module
import Test.Cores.Ethernet.Util

-- upconverter model
import Test.Cores.Ethernet.UpConverter qualified as UC

-- downconverter model
import Test.Cores.Ethernet.DownConverter qualified as DC

-- data module
import Data.List qualified as L
import Data.Maybe

-- crc module
import Clash.Cores.Crc
import Clash.Cores.Crc.Catalog

-- fcs inserter
import Clash.Cores.Ethernet.FcsInserter

-- packetstream
import Clash.Cores.Ethernet.PacketStream

-- proxy
import Data.Proxy

genVec :: (C.KnownNat n, 1 C.<= n) => Gen a -> Gen (C.Vec n a)
genVec gen = sequence (C.repeat gen)

model
  :: C.KnownNat dataWidth
  => 1 C.<= dataWidth
  => [PacketStreamM2S dataWidth ()] -> [PacketStreamM2S dataWidth ()]
model fragments = insertCrc =<< chunkByPacket fragments

packetToCrcInp
  :: C.KnownNat dataWidth
  => 1 C.<= dataWidth
  => [PacketStreamM2S dataWidth ()] -> [C.BitVector 8]
packetToCrcInp packet = C.head . _data <$> (chopPacket =<< packet)

insertCrc
  :: forall (dataWidth :: C.Nat)
  .  C.KnownNat dataWidth
  => 1 C.<= dataWidth
  => [PacketStreamM2S dataWidth ()] -> [PacketStreamM2S dataWidth ()]
insertCrc =  upConvert . go . downConvert
  where
    go :: [PacketStreamM2S 1 ()] -> [PacketStreamM2S 1 ()]
    go pkt = pkt''
      where
        crcInp = C.head . _data <$> pkt
        softwareCrc = mkSoftwareCrc (Proxy @Crc32_ethernet) C.d8
        crc = digest $ L.foldl' feed softwareCrc  crcInp
        crc' = C.singleton . C.v2bv <$> (C.toList . C.reverse . C.unconcat C.d8 . C.bv2v $ crc)
        lastfmnt = L.last pkt
        pkt' = init pkt L.++ [lastfmnt {_last = Nothing}] L.++ fmap (\dat -> lastfmnt {_data = dat, _last = Nothing}) crc'
        pkt'' = init pkt' ++ [(last pkt'){_last = Just 0}]

-- | Test the fcsinserter
fcsinserterTest
  :: forall n
  .  (1 C.<= n, HardwareCrc Crc32_ethernet 8 n)
  => C.SNat n -> Property
fcsinserterTest C.SNat =
  propWithModelSingleDomain
    @C.System
    defExpectOptions
    (fmap fullPackets (Gen.list (Range.linear 0 100) genPackets))  -- Input packets
    (C.exposeClockResetEnable model)                               -- Desired behaviour of FcsInserter
    (C.exposeClockResetEnable ckt)                                 -- Implementation of FcsInserter
    (===)                                                          -- Property to test
  where
    ckt
      :: forall (dom :: C.Domain) (dataWidth :: C.Nat)
      .  C.KnownDomain dom
      => 1 C.<= dataWidth
      => C.HiddenClockResetEnable dom
      => HardwareCrc Crc32_ethernet 8 dataWidth
      => Circuit
        (PacketStream dom dataWidth ())
        (PacketStream dom dataWidth ())
    ckt = fcsInserterC

    -- This generates the packets
    genPackets =
      PacketStreamM2S <$>
      genVec @n Gen.enumBounded <*>
      Gen.maybe Gen.enumBounded <*>
      Gen.enumBounded <*>
      Gen.enumBounded

$(deriveHardwareCrc (Proxy @Crc32_ethernet) C.d8 C.d1)
$(deriveHardwareCrc (Proxy @Crc32_ethernet) C.d8 C.d2)
$(deriveHardwareCrc (Proxy @Crc32_ethernet) C.d8 C.d4)
$(deriveHardwareCrc (Proxy @Crc32_ethernet) C.d8 C.d8)

prop_fcsinserter_d1, prop_fcsinserter_d2, prop_fcsinserter_d4, prop_fcsinserter_d8 :: Property
prop_fcsinserter_d1 = fcsinserterTest C.d1
prop_fcsinserter_d2 = fcsinserterTest C.d2
prop_fcsinserter_d4 = fcsinserterTest C.d4
prop_fcsinserter_d8 = fcsinserterTest C.d8

tests :: TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1_000))
  $(testGroupGenerator)
