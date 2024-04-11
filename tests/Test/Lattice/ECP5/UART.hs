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
import Protocols.Internal (CSignal(CSignal))
import Protocols.Hedgehog

-- Me
import Clash.Cores.Ethernet.PacketStream
import Clash.Lattice.ECP5.UART
import qualified Protocols.DfConv as DfConv
import Test.Cores.Ethernet.Util

-- | Tests that data can be sent through UART, except for _abort and _last and _meta signals. Ignores backpressure.
prop_uart_tx_rx_id :: Property
prop_uart_tx_rx_id = idWithModelSingleDomain @C.System defExpectOptions gen (C.exposeClockResetEnable id) ckt
  where
    ckt = C.exposeClockResetEnable (
      uartTxC @C.System (C.SNat @6250000)
      |> uartRxC (C.SNat @6250000)
      |> unsafeByteToPacketStream
      |> DfConv.fifo (Proxy :: Proxy (PacketStream C.System 1 ())) (Proxy :: Proxy (PacketStream C.System 1 ())) (C.SNat @50)
      )

    gen :: Gen (ExpectType (PacketStream dom 1 ()))
    gen = Gen.list (Range.linear 0 50) genPackets
    genPackets =
      PacketStreamM2S <$>
      genVec Gen.enumBounded <*>
      pure Nothing <*>
      Gen.enumBounded <*>
      pure False

    unsafeByteToPacketStream :: Circuit (CSignal dom (Maybe (C.BitVector 8))) (PacketStream dom 1 ())
    unsafeByteToPacketStream = Circuit (\(CSignal fwdInS, _) -> (CSignal $ pure (), go fwdInS))
      where
        go = fmap $ fmap $ \byte -> PacketStreamM2S (C.singleton byte) Nothing () False

-- Model for `toPacketsC`.
toPackets :: [C.BitVector 8] -> [PacketStreamM2S 1 ()]
toPackets (l0 : (l1 : xs)) = packet ++ toPackets xs' where
  len = 1 + fromIntegral (l1 C.++# l0)
  (packet, xs') = splitPacketAt len xs
toPackets _ = []

-- | Like `splitAt`, but turns bytes into PacketStreamM2S with _last set correctly.
splitPacketAt :: Int -> [C.BitVector 8] -> ([PacketStreamM2S 1 ()], [C.BitVector 8])
splitPacketAt n ls
  | n <= 0 = ([], ls)
  | otherwise = splitAt' n ls
    where
      splitAt' _ [] = ([], [])
      splitAt' 1 (x : xs) = ([PacketStreamM2S (C.singleton x) (Just 0) () False], xs)
      splitAt' m (x : xs) = (PacketStreamM2S (C.singleton x) Nothing () False : xs', xs'')
        where
          (xs', xs'') = splitAt' (m - 1) xs

-- | Tests that `Clash.Lattice.ECP5.UART.toPacketsC` removes size bytes, sets _last correctly, and leaves _abort and _meta as is on 10000 random inputs.
prop_topackets :: Property
prop_topackets = property $ do
  let n = 10_000
      gen = Gen.list (Range.linear 0 n) $ Gen.maybe Gen.enumBounded
  packets <- forAll (gen :: Gen [Maybe (C.BitVector 8)])
  let ckt = C.exposeClockResetEnable @C.System toPacketsC C.clockGen C.resetGen C.enableGen
      throughCkt = catMaybes $ take (n + 1) $ simulateCS ckt (Nothing : packets L.++ L.repeat Nothing)
      throughModel = toPackets (catMaybes packets)
  throughModel === throughCkt

tests :: TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 200))
  $(testGroupGenerator)
