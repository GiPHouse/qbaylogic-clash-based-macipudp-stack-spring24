{-# language FlexibleContexts #-}
{-# language NumericUnderscores #-}
{-# language RecordWildCards #-}

module Test.Cores.Ethernet.Icmp where

-- base
import Prelude

-- clash-prelude
import Clash.Prelude hiding ( concatMap )
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
import Protocols.Extra.PacketStream
import Protocols.Hedgehog

-- ethernet
import Clash.Cores.Ethernet.Icmp
import Clash.Cores.Ethernet.Icmp.IcmpTypes
import Clash.Cores.Ethernet.IP.InternetChecksum ( onesComplementAdd )
import Clash.Cores.Ethernet.IP.IPv4Types

-- tests
import Test.Protocols.Extra.PacketStream.Extra
import Test.Protocols.Extra.PacketStream.Packetizers ( depacketizerModel, packetizerModel )


genVec :: (C.KnownNat n, 1 <= n) => Gen a -> Gen (C.Vec n a)
genVec gen = sequence (C.repeat gen)

genIpAddr :: Gen IPv4Address
genIpAddr = IPv4Address <$> C.sequence (C.repeat @4 Gen.enumBounded)

ourIpAddr :: IPv4Address
ourIpAddr = IPv4Address (C.repeat @4 0x3)

genRandomWord :: Gen (PacketStreamM2S 4 (IPv4HeaderLite, IcmpHeaderLite))
genRandomWord = do
  icmp <- IcmpHeaderLite 0x8 <$> Gen.enumBounded
  ipv4 <- IPv4HeaderLite <$> genIpAddr <*> pure ourIpAddr <*> pure 2
  dat <- genVec Gen.enumBounded

  return $ PacketStreamM2S dat Nothing (ipv4, icmp) False

genRandomPacket :: Gen [PacketStreamM2S 4 IPv4HeaderLite]
genRandomPacket = packetizerModel fst snd <$> genValidPacket (Range.linear 0 10) genRandomWord

depacketize :: [PacketStreamM2S 4 IPv4HeaderLite] -> [PacketStreamM2S 4 (IPv4HeaderLite, IcmpHeaderLite)]
depacketize = depacketizerModel f
  where
    f :: IcmpHeader -> IPv4HeaderLite -> (IPv4HeaderLite, IcmpHeaderLite)
    f IcmpHeader{..} ipheader = (ipheader,  IcmpHeaderLite{_typeL = _type, _checksumL = _checksum})

icmpReceiverPropertyGenerator
  :: forall (dataWidth :: Nat).
  1 <= dataWidth
  => SNat dataWidth
  -> Property
icmpReceiverPropertyGenerator C.SNat =
  propWithModelSingleDomain
    @C.System
    defExpectOptions
    (fmap fullPackets (Gen.list (Range.linear 1 100) (genPackets genIPv4HeaderLite)))
    (C.exposeClockResetEnable model)
    (C.exposeClockResetEnable @C.System icmpReceiverC)
    (===)
    where
      f :: IcmpHeader -> IPv4HeaderLite -> (IPv4HeaderLite, IcmpHeaderLite)
      f IcmpHeader{..} ipheader = (ipheader,  IcmpHeaderLite{_typeL = _type, _checksumL = _checksum})

      model :: [PacketStreamM2S dataWidth IPv4HeaderLite] -> [PacketStreamM2S dataWidth (IPv4HeaderLite, IcmpHeaderLite)]
      model = depacketizerModel f

      genPackets :: Gen IPv4HeaderLite -> Gen (PacketStreamM2S dataWidth IPv4HeaderLite)
      genPackets genMeta =
        PacketStreamM2S <$>
        genVec Gen.enumBounded <*>
        Gen.maybe Gen.enumBounded <*>
        genMeta <*>
        Gen.enumBounded

      testAddress :: Gen IPv4Address
      testAddress = pure $ IPv4Address (C.repeat 0x00)

      genIPv4HeaderLite :: Gen IPv4HeaderLite
      genIPv4HeaderLite = IPv4HeaderLite <$> testAddress <*> testAddress <*> pure 0

prop_icmp_receiver_d1 :: Property
prop_icmp_receiver_d1 = icmpReceiverPropertyGenerator d1

prop_icmp_receiver_d2 :: Property
prop_icmp_receiver_d2 = icmpReceiverPropertyGenerator d2

prop_icmp_receiver_d3 :: Property
prop_icmp_receiver_d3 = icmpReceiverPropertyGenerator d3

prop_icmp_receiver_d4 :: Property
prop_icmp_receiver_d4 = icmpReceiverPropertyGenerator d4

-- | A  test for the working of the echo responder component
-- The correct updating of the checksum should be tested in hardware
prop_icmp_echoResponder_response_type :: Property
prop_icmp_echoResponder_response_type =
    idWithModelSingleDomain
    @C.System
    defExpectOptions
    genRandomPacket
    (C.exposeClockResetEnable model)
    (C.exposeClockResetEnable ckt)
 where
  ckt :: C.HiddenClockResetEnable C.System => Circuit (PacketStream C.System 4 IPv4HeaderLite) (PacketStream C.System 4 IPv4HeaderLite)
  ckt = icmpEchoResponderC $ pure ourIpAddr

  model :: [PacketStreamM2S 4 IPv4HeaderLite] -> [PacketStreamM2S 4 IPv4HeaderLite]
  model packets = packetizerModel fst snd $ swapAdresses . adjustIcmpType <$> depacketized
    where
      depacketized = depacketize packets

      swapAdresses ::
        PacketStreamM2S 4 (IPv4HeaderLite, IcmpHeaderLite)
        -> PacketStreamM2S 4 (IPv4HeaderLite, IcmpHeaderLite)
      swapAdresses = fmap (\(ip@IPv4HeaderLite{..}, icmp)
        -> (ip{_ipv4lSource = ourIpAddr, _ipv4lDestination = _ipv4lSource}, icmp))

      adjustIcmpType ::
        PacketStreamM2S 4 (IPv4HeaderLite, IcmpHeaderLite)
        -> PacketStreamM2S 4 (IPv4HeaderLite, IcmpHeaderLite)
      adjustIcmpType = fmap (\(ip, icmp@IcmpHeaderLite{..}) -> (ip, icmp{
        _checksumL = onesComplementAdd (complement 0x0800) _checksumL,
        _typeL = 0
        }))

tests :: TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1_000))
  $(testGroupGenerator)
