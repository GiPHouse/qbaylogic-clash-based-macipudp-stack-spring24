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
import Protocols.Hedgehog

-- Me
import Clash.Cores.Ethernet.PacketStream
import Clash.Lattice.ECP5.UART
import qualified Protocols.DfConv as DfConv

genVec :: (C.KnownNat n, 1 <= n) => Gen a -> Gen (C.Vec n a)
genVec gen = sequence (C.repeat gen)

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

-- Model for `toPacketsC`.
toPackets :: [PacketStreamM2S 1 a] -> [PacketStreamM2S 1 a]
toPackets (l0 : (l1 : xs)) = packet ++ toPackets xs' where
  len = 1 + fromIntegral (C.head (_data l1) C.++# C.head (_data l0))
  (packet, xs') = splitPacketAt len xs
toPackets _ = []

-- | Like `splitAt`, but also sets _last.
splitPacketAt :: Int -> [PacketStreamM2S 1 a] -> ([PacketStreamM2S 1 a], [PacketStreamM2S 1 a])
splitPacketAt n ls
  | n <= 0 = ([], ls)
  | otherwise = splitAt' n ls
    where
      splitAt' _ [] = ([], [])
      splitAt' 1 (x : xs) = ([x {_last = Just 0}], xs)
      splitAt' m (x : xs) = ((x {_last = Nothing}) :xs', xs'')
        where
          (xs', xs'') = splitAt' (m - 1) xs

-- | Tests that `Clash.Lattice.ECP5.UART.toPacketsC` removes size bytes, sets _last correctly, and leaves _abort and _meta as is on 10000 random inputs.
prop_topackets :: Property
prop_topackets = property $ do
  let n = 10_000
      gen = Gen.list (Range.linear 0 n) $
        Gen.maybe $
        PacketStreamM2S <$>
        genVec Gen.enumBounded <*>
        Gen.maybe Gen.enumBounded <*>
        Gen.enumBounded <*>
        Gen.enumBounded
  packets <- forAll (gen :: Gen [Maybe (PacketStreamM2S 1 Int)])
  let ckt = C.exposeClockResetEnable @C.System toPacketsC C.clockGen C.resetGen C.enableGen
      throughCkt = catMaybes $ take (n + 1) $ simulateCS ckt (Nothing : packets L.++ L.repeat Nothing)
      throughModel = toPackets (catMaybes packets)
  throughModel === throughCkt

tests :: TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 200))
  $(testGroupGenerator)
