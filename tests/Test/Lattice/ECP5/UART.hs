{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Lattice.ECP5.UART where

-- base
import Prelude
import Data.Proxy
import qualified Data.List as L
import Data.Maybe

-- clash-prelude
import qualified Clash.Prelude as C
import Clash.Prelude (type (<=))

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
import Protocols.Internal
import Protocols.Hedgehog

-- Me
import Clash.Cores.Ethernet.PacketStream
import Clash.Lattice.ECP5.UART
import qualified Protocols.DfConv as DfConv

genVec :: (C.KnownNat n, 1 <= n) => Gen a -> Gen (C.Vec n a)
genVec gen = sequence (C.repeat gen)

unsafeToPacketStream :: Circuit (CSignal dom (Maybe (PacketStreamM2S 1 ()))) (PacketStream dom 1 ())
unsafeToPacketStream = Circuit (\(CSignal fwdInS, _) -> (CSignal $ pure (), fwdInS))

-- | Tests that data can be sent through UART, except for _abort and _last and _meta signals. Ignores backpressure.
prop_uart_tx_rx_id :: Property
prop_uart_tx_rx_id = idWithModelSingleDomain @C.System defExpectOptions gen (C.exposeClockResetEnable model) ckt
  where
    ckt = C.exposeClockResetEnable (
      uartTxC @C.System (C.SNat @6250000) 
      |> uartRxC (C.SNat @6250000) 
      |> unsafeToPacketStream
      |> DfConv.fifo (Proxy :: Proxy (PacketStream C.System 1 ())) (Proxy :: Proxy (PacketStream C.System 1 ())) (C.SNat @50)
      )

    model :: C.HiddenClockResetEnable dom => ExpectType (PacketStream dom 1 ()) -> ExpectType (PacketStream dom 1 ())
    model = fmap (\x -> x {_abort=False, _last=Nothing})

    gen :: Gen (ExpectType (PacketStream dom 1 ()))
    gen = Gen.list (Range.linear 0 50) genPackets

    genPackets =
      PacketStreamM2S <$>
      genVec Gen.enumBounded <*>
      Gen.maybe Gen.enumBounded <*>
      Gen.enumBounded <*>
      Gen.enumBounded

toPackets :: [PacketStreamM2S 1 ()] -> [[PacketStreamM2S 1 ()]]
toPackets (l0 : (l1 : xs)) = packet : toPackets rest where
  len = C.head (_data l1) C.++# C.head (_data l0)
  (packetRaw, rest) = L.splitAt (1 + fromIntegral len) xs
  packet = setLast packetRaw
  setLast :: [PacketStreamM2S 1 ()] -> [PacketStreamM2S 1 ()]
  setLast [] = []
  setLast [z] = [z {_last = Just 0}]
  setLast (z : zs) = z {_last = Nothing} : setLast zs
toPackets _ = []

-- | Tests that `toPacketsC` removes size bytes and sets _last correctly on 10000 random inputs.
prop_topackets :: Property
prop_topackets = property $ do
  packets <- forAll (Gen.list (Range.linear 0 10_000) $ Gen.maybe (PacketStreamM2S <$>
    genVec Gen.enumBounded <*>
    Gen.maybe Gen.enumBounded <*>
    Gen.enumBounded <*>
    Gen.enumBounded))
  let ckt = C.exposeClockResetEnable @C.System toPacketsC C.clockGen C.resetGen C.enableGen
  let throughCkt = catMaybes $ take 10_000 $ drop 1 $ simulateCS ckt (Nothing : packets L.++ L.repeat Nothing)
  let throughModel = concat (toPackets $ catMaybes packets)
  throughModel === throughCkt

tests :: TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 200))
  $(testGroupGenerator)
