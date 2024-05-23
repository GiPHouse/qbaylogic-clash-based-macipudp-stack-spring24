{-# language FlexibleContexts #-}
{-# language NumericUnderscores #-}
{-# language RecordWildCards #-}

module Test.Cores.Ethernet.IP.Icmp where

-- base
import Data.Maybe
import Prelude

-- clash-prelude
import Clash.Prelude qualified as C

-- hedgehog
import Hedgehog
import Hedgehog.Gen qualified as Gen

-- tasty
import Hedgehog.Range qualified as Range
import Test.Tasty
import Test.Tasty.Hedgehog ( HedgehogTestLimit(HedgehogTestLimit) )
import Test.Tasty.Hedgehog.Extra ( testProperty )
import Test.Tasty.TH ( testGroupGenerator )

-- clash-cores
import Clash.Cores.Ethernet.InternetChecksum ( onesComplementAdd )
import Clash.Cores.Ethernet.PacketStream
import Clash.Cores.IP.Icmp
import Clash.Cores.IP.IcmpTypes
import Clash.Cores.IP.IPv4Types
import Clash.Explicit.Prelude ( complement )
import Protocols
import Protocols.Hedgehog
import Test.Cores.Ethernet.Depacketizer ( depacketizerModel )
import Test.Cores.Ethernet.Packetizer ( packetizerModel )
import Test.Cores.Ethernet.Util ( genValidPacket )

genIpAddr :: Gen IPv4Address
genIpAddr = IPv4Address <$> C.sequence (C.repeat @4 Gen.enumBounded)

intToIPv4Addr :: C.BitVector 8 -> IPv4Address
intToIPv4Addr i = IPv4Address (C.repeat @4 i)

ourIpAddr :: IPv4Address
ourIpAddr = intToIPv4Addr 0x3

genVec :: (C.KnownNat n, 1 C.<= n) => Gen a -> Gen (C.Vec n a)
genVec gen = sequence (C.repeat gen)

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
  ckt = icmpEchoResponderC $ pure (intToIPv4Addr 3)

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
