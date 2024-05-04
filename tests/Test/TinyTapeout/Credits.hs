{-# language FlexibleContexts #-}
{-# language NumericUnderscores #-}
{-# language RecordWildCards #-}

module Test.TinyTapeout.Credits where

-- base
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

-- clash-protocols
import Protocols
import Protocols.Hedgehog

-- util module
import Test.Cores.Ethernet.Util

-- ethernet modules
import Clash.Cores.Ethernet.PacketStream
import Clash.Cores.Ethernet.EthernetTypes
import Clash.TinyTapeout.EthernetMac.Credits (creditsBV)
import Clash.TinyTapeout.EthernetMac.EthStack (creditsC)

genVec :: (C.KnownNat n, 1 <= n) => Gen a -> Gen (C.Vec n a)
genVec gen = sequence (C.repeat gen)

model :: forall n meta. 1 <= n => C.KnownNat n => [PacketStreamM2S n meta] -> [PacketStreamM2S n meta]
model fragments = concatMap creditPacket $ chunkByPacket fragments
  where
    creditPacket pkt = fullPackets
                         $ fmap (\b -> lastF { _data = C.unpack b, _last = Nothing })
                         $ creditsBV $ C.SNat @n
      where
        lastF = last pkt

-- | Test the downconverter stream instance
prop_credit :: Property
prop_credit =
  propWithModelSingleDomain
    @C.System
    defExpectOptions
    (fmap fullPackets $ Gen.list (Range.linear 0 100) genPackets)  -- Input packets
    (C.exposeClockResetEnable model)                               -- Desired behaviour of DownConverter
    (C.exposeClockResetEnable @C.System creditsC)                  -- Implementation of DownConverter
    (===)                                                          -- Property to test
  where
    -- This generates the packets
    genPackets =
      PacketStreamM2S <$>
      genVec Gen.enumBounded <*>
      Gen.maybe Gen.enumBounded <*>
      (pure $ EthernetHeader (MacAddress $ C.repeat 0xff) (MacAddress $ C.repeat 0xff) 0) <*>
      Gen.enumBounded

tests :: TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1_000))
  $(testGroupGenerator)
