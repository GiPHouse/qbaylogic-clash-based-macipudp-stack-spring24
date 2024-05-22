{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Test.TinyTapeout.IcmpEcho where

-- base
import Control.Monad
import Data.Bits (complement)
import Data.List (foldl', intersperse)
import Data.Maybe (catMaybes)
import Numeric (showHex)
import Prelude

import Clash.Hedgehog.Sized.Index
import Clash.Hedgehog.Sized.Unsigned

-- clash-prelude
import Clash.Prelude ( type (<=) )
import Clash.Prelude qualified as C

-- hedgehog
import Hedgehog qualified as H
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
import Clash.TinyTapeout.EthernetMac.IcmpEcho


-- Functionality test: does the component generate the correct response packets?
-- Both packets that should and should not be responded to are generated, based
-- on `alter`, and streams of packets are generated to test the state machine
-- correctly transitions to a new packet at the end of the current packet.
prop_basic :: H.Property
prop_basic = H.property $ do
  alters <- H.forAll $ Gen.list (Range.linear 0 10) (Gen.maybe Gen.enumBounded)
  -- alters <- H.forAll $ Gen.list (Range.linear 0 10) (Gen.constant Nothing)
  (inPkts, expectedPkts) <-
    mapAndUnzipM (H.forAllWith pPrintPair . genPacket) alters
  let
    inStream = concatMap (packetToStream ()) inPkts
    outStream = simulateIcmpEchoC inStream
    outPkts = map streamToPacket $ chunkByPacket outStream
  H.footnote $ pPrintPacketsIEO (zip inPkts expectedPkts) outPkts ""
  H.assert (outPkts == expectedPkts)
  where
    pPrintPair (inp, ex) = pPrintPacketIE inp ex ""

-- This uses `propWithModel` purely to test the stream invariants. The "model"
-- is just the component run without backpressure. If `prop_basic` passed, that
-- should lead to the correct "model". Testing the actual functionality with
-- `Protocols.Hedgehog` gave really awful formatting of counter-examples, which
-- is why `prop_basic` was implemented so you can actually find what went
-- wrong with it on a counter-example. Testing the stream invariants is useful
-- in itself, which is why this test exists as well.
prop_stream :: H.Property
prop_stream =
  propWithModelSingleDomain
    @C.System defExpectOptions
    (do
      alters <- Gen.list (Range.linear 1 10) (Gen.constant Nothing)
      (inPkts, _expectedPkts) <- mapAndUnzipM genPacket alters
      pure $ concatMap (packetToStream ()) inPkts
    )
    (\_clk _rst _en inp -> simulateIcmpEchoC inp)
    (C.exposeClockResetEnable icmpEchoC)
    (H.===)

-- Run the component without backpressure, ending simulation on a 1000 empty
-- outputs
simulateIcmpEchoC
  :: forall meta
   . [PacketStreamM2S DataWidth meta]
  -> [PacketStreamM2S DataWidth meta]
simulateIcmpEchoC = catMaybes . simulateC comp simConfig . map Just
  where
  comp =
    C.withClockResetEnable C.systemClockGen C.resetGen C.enableGen icmpEchoC
  simConfig = def{timeoutAfter=1000, ignoreReset=True}

-- Returns [] when the stream was aborted
streamToPacket
  :: forall n meta
   . C.KnownNat n
  => 1 <= n
  => [PacketStreamM2S n meta]
  -> [C.Unsigned 8]
streamToPacket inp
  | or $ map _abort inp
  = []
  | otherwise
  = map (C.unpack . C.head . _data) $ downConvert inp

packetToStream
  :: forall n meta
   . C.KnownNat n
  => 1 <= n
  => meta
  -> [C.Unsigned 8]
  -> [PacketStreamM2S n meta]
packetToStream _meta [] = []
packetToStream meta inPkt =
  upConvert $ map nonFinal (init inPkt) ++ [final (last inPkt)]
  where
    nonFinal b =
      PacketStreamM2S
        { _data = C.singleton $ C.pack b
        , _last = Nothing
        , _meta = meta
        , _abort = False
        }
    final b = (nonFinal b){_last = Just maxBound}

data AlterPacket
  = AlterVerLen
    -- ^ Alter the IPv4 version field or the Internet Header Length field
  | AlterL3Proto
    -- ^ Alter the level 3 protocol
  | AlterIpType
    -- ^ Alter the destination IP address or the ICMP Type/Code
  deriving (Enum, Bounded, Show)

-- If the request should not be answered, the reply packet is [].
genPacket
  :: H.MonadGen m
  => Maybe AlterPacket ->
  -- | (ICMP echo request packet, ICMP echo reply packet)
  m ([C.Unsigned 8], [C.Unsigned 8])
genPacket alter = do
  verLen1 <-
    case alter of
      Just AlterVerLen -> genUnsigned Range.constantBounded
      _ -> pure verLen
  l3proto1 <-
    case alter of
      Just AlterL3Proto -> genUnsigned Range.constantBounded
      _ -> pure l3proto
  ipType1 <-
    case alter of
      Just AlterIpType -> do
        pos <- genIndex @_ @6 Range.constantBounded
        byte <- genUnsigned Range.constantBounded
        pure (replaceByte pos byte ipType)
      _ -> pure ipType
  random2 <- replicateM 8 $ genUnsigned Range.constantBounded
  srcIp <- replicateM 4 $ genUnsigned Range.constantBounded
  random3 <- replicateM 2 $ genUnsigned Range.constantBounded
  extraLen <- Gen.integral (Range.linear 0 64)
  random5 <- replicateM (extraLen + 4) $ genUnsigned Range.constantBounded
  let
    request cksum =
      verLen1 : random2 ++ [l3proto1] ++ random3 ++
      srcIp ++ ipType1 ++ cksum ++ random5
    request1 = request $ u16toU8s $ checksumIcmp $ request [0,0]
    reply cksum =
      verLen1 : random2 ++ [l3proto] ++ random3 ++
      take 4 ipType ++ srcIp ++ [0x00, 0x00] ++ cksum ++ random5

    reply1 = reply $ u16toU8s $ checksumIcmp $ reply [0,0]
    reply2
      | verLen1 == verLen
      , l3proto1 == l3proto
      , ipType1 == ipType
      = reply1
      | otherwise = []

  pure (request1, reply2)
 where
  verLen = 0x45
  l3proto = 0x01
  ipType = [ 0x0a, 0x00, 0x00, 0x02, 0x08, 0x00 ]

  replaceByte pos byte xs =
    let (xs1, xs2) = splitAt (fromIntegral pos) xs
    in xs1 ++ (byte:tail xs2)

-- Input is a whole IP packet, output is the checksum of the ICMP part of the
-- packet
checksumIcmp
  :: [C.Unsigned 8]
  -> C.Unsigned 16
checksumIcmp xs =
  complement $ foldl' (~+~) 0 $ map to16 $ parts $ take 8 $ drop 20 xs
 where
  parts []  = []
  parts xs1 = let (xs2, xs3) = splitAt 2 xs1 in xs2:parts xs3
  to16 :: [C.Unsigned 8] -> C.Unsigned 16
  to16 xs1 =
    let
      hi = C.resize $ head xs1
      lo = C.resize $ head (tail xs1)
    in 256 * hi + lo

u16toU8s
  :: C.Unsigned 16
  -> [C.Unsigned 8]
u16toU8s n = [ C.resize (n `C.shiftR` 8), C.resize n ]

pPrintPacket
  :: [C.Unsigned 8]
  -> ShowS
pPrintPacket [] = ("No packet\n" ++)
pPrintPacket packet
  | length packet < 28
  = ("Runt packet\nDump:\n" ++) . showDump packet . ('\n':)
  | otherwise
  = ("IP ver/len: " ++) . showHex8 verLen . ("\nL3 proto: " ++) .
    showHex8 l3Proto . ("\nSrc IP: " ++) . showHexList srcIp .
    ("\nDst IP: " ++) . showHexList destIp . ("\nICMP Type: " ++) .
    showHex8 icmpType . ("\nPayload length: " ++) . shows payLen .
    ("\nComputed checksum: " ++) . showHex16 checksum . ("\nHex dump:\n" ++) .
    showDump packet . ('\n':)
 where
  verLen = head packet
  packet4 = drop 9 packet
  l3Proto = head packet4
  packet5 = tail packet4
  (srcIp, packet6) = splitAt 4 $ drop 2 packet5
  (destIp, packet7) = splitAt 4 packet6
  icmpType = head packet7
  payLen = length packet7 - 8
  checksum = checksumIcmp packet

  showHexAlign p n =
    let cs = showHex n ""
    in ((replicate (p - length cs) '0' ++ cs) ++)

  showHex8 = showHexAlign 2
  showHex16 = showHexAlign 4

  showHexList ns =
    foldr (.) id $ intersperse (' ':) $ map showHex8 ns

  showDump ns =
    foldr (.) id $ intersperse ('\n':) $ map showHexList $ parts ns
   where
    parts [] = []
    parts ns1 = let (ns2, ns3) = splitAt 8 ns1 in ns2:parts ns3


pPrintPackets
  :: [[C.Unsigned 8]]
  -> ShowS
pPrintPackets = go (0 :: Int)
  where
    go _ [] = id
    go n (pkt:pkts) =
      ("Packet " ++) . shows n . (":\n" ++) . pPrintPacket pkt . ('\n':) .
      go (n+1) pkts

-- IE = Input, Expected
pPrintPacketIE
  :: [C.Unsigned 8]
  -> [C.Unsigned 8]
  -> ShowS
pPrintPacketIE inp ex =
  ("Input packet:\n" ++) . pPrintPacket inp . ("\nExpected packet:\n" ++) .
  pPrintPacket ex

pPrintPacketsIE
  :: [ ( [C.Unsigned 8]
      , [C.Unsigned 8]
      )]
  -> ShowS
pPrintPacketsIE = go (0 :: Int)
  where
    go _ [] = id
    go n ((inp, ex):inpExps0) =
      fmt "Input packet" n inp . fmt "Expected packet" n ex . go (n+1) inpExps0

    fmt hdr n pkt =
      ((hdr ++ " ") ++) . shows n . (":\n" ++) . pPrintPacket pkt . ('\n':)

-- IEO = Input, Expected, Output
pPrintPacketIEO
  :: [C.Unsigned 8]
  -> [C.Unsigned 8]
  -> [C.Unsigned 8]
  -> ShowS
pPrintPacketIEO inp ex out =
  ("Input packet:\n" ++) . pPrintPacket inp . ("\nExpected packet:\n" ++) .
  pPrintPacket ex . ("\nOutput packet:\n" ++) . pPrintPacket out

pPrintPacketsIEO ::
  [ ( [C.Unsigned 8]
    , [C.Unsigned 8]
    )] ->
  [[C.Unsigned 8]] ->
  ShowS
pPrintPacketsIEO inpExps outs = go (0 :: Int) inpExps (map Just outs)
  where
    go _ [] [] = id
    go _ [] (Nothing:_) = id
    go n [] ((Just out):outs0) =
      fmt "Extraneous output packet" n out . go (n+1) [] outs0
    go n inpExps0@(_:_) [] = go n inpExps0 (replicate (length inpExps0) Nothing)
    go n ((inp, ex):inpExps0) (out:outs0) =
      fmt "Input packet" n inp . fmt "Expected packet" n ex . fmtOut out .
      go (n+1) inpExps0 outs0
     where
      fmtOut Nothing = ("Missing output packet\n" ++)
      fmtOut (Just pkt) = fmt "Output packet" n pkt

    fmt hdr n pkt =
      ((hdr ++ " ") ++) . shows n . (":\n" ++) . pPrintPacket pkt . ('\n':)

tests :: TestTree
tests =
--     localOption (mkTimeout 12_000_000 {- 12 seconds -})
--   $ localOption (HedgehogTestLimit (Just 1_000))
  $(testGroupGenerator)
