{-# language FlexibleContexts #-}
{-# language NumericUnderscores #-}
{-# language RecordWildCards #-}

module Test.Cores.Ethernet.PadPacket where

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
import Clash.Cores.Ethernet.PadPacket

genVec :: (C.KnownNat n, 1 <= n) => Gen a -> Gen (C.Vec n a)
genVec gen = sequence (C.repeat gen)

model
  :: forall (dataWidth :: C.Nat).
  1 <= dataWidth
  => C.KnownNat dataWidth
  => [PacketStreamM2S dataWidth ()]
  -> [PacketStreamM2S dataWidth ()]
model fragments = concatMap (fixLast . map (\x -> x {_last = Nothing}) . padPacket) (chunkByPacket fragments)
  where
    padPacket pkt = pkt ++ replicate (neededPadding pkt) padding
    neededPadding pkt = max 0 (div (64 + (C.natToNum @dataWidth) - 1) (C.natToNum @dataWidth) - length pkt)
    padding = PacketStreamM2S {_data = C.repeat 0, _last = Nothing, _meta = (), _abort = False}
    lastIndex :: C.Index 64
    lastIndex = mod (63 - C.natToNum @dataWidth) (C.natToNum @dataWidth)
    fixLast pkt = init pkt ++ [(last pkt) {_last = Just (C.resize lastIndex)}]

-- | Test the padding inserter
padpacketTest :: forall n. 1 <= n => C.SNat n -> Property
padpacketTest C.SNat =
  propWithModelSingleDomain
    @C.System
    defExpectOptions
    (fmap fullPackets (Gen.list (Range.linear 0 100) genPackets))
    (C.exposeClockResetEnable model)
    (C.exposeClockResetEnable @C.System (ckt @n))
    (===)
  where
    ckt :: forall (dataWidth :: C.Nat) (dom :: C.Domain).
      C.HiddenClockResetEnable dom
      => 1 <= dataWidth
      => C.KnownNat dataWidth
      => Circuit (PacketStream dom dataWidth ()) (PacketStream dom dataWidth ())
    ckt = padPacketC

    -- This generates the packets
    genPackets =
      PacketStreamM2S <$>
      genVec Gen.enumBounded <*>
      Gen.maybe Gen.enumBounded <*>
      Gen.enumBounded <*>
      Gen.enumBounded

prop_padpacket_d1, prop_padpacket_d2, prop_padpacket_d4 :: Property
prop_padpacket_d1 = padpacketTest (C.SNat @1)
prop_padpacket_d2 = padpacketTest (C.SNat @2)
prop_padpacket_d4 = padpacketTest (C.SNat @5)

tests :: TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1_000))
  $(testGroupGenerator)
