{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Cores.Ethernet.UpConverter where

-- base
import Prelude

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
import Clash.Cores.Ethernet.UpConverter
import Clash.Cores.Ethernet.PacketStream

genVec :: (C.KnownNat n, 1 <= n) => Gen a -> Gen (C.Vec n a)
genVec gen = sequence (C.repeat gen)

-- | Test the upconverter stream instance
--   TODO: Use the fifo given by `DfConv`
prop_packetstream_sometest_id :: Property
prop_packetstream_sometest_id =
  propWithModelSingleDomain
    @C.System
    defExpectOptions
    (Gen.list (Range.linear 0 100) genPackets)
    (C.exposeClockResetEnable $ go)
    (C.exposeClockResetEnable @C.System (ckt @4))
    (\a b -> a === b)
  where
    ckt :: forall (dataWidth :: C.Nat) (dom :: C.Domain).
      C.HiddenClockResetEnable dom
      => 1 <= dataWidth
      => C.KnownNat dataWidth
      => Circuit (PacketStream dom 1 ()) (PacketStream dom dataWidth ())
    ckt = upConverterC

    go :: UpConverterState dataWidth
      -> (Maybe (PacketStreamM2S 1 ()))
      -> ( UpConverterState dataWidth
         , Maybe (PacketStreamM2S dataWidth ())
         )
    go st@(UpConverterState {..}) (Nothing)
--   = (nextSt, (toPacketStream st))
--     where
--       -- If we can accept data we can always set _ucFlush to false,
--       -- since we only change state if we can transmit and receive data
--       nextStRaw = st
--                     { _ucFlush = False
--                     , _ucAborted = isNothing _ucLastIdx && _ucAborted
--                     , _ucLastIdx = Nothing
--                     }
--       outReady = not _ucFlush || inReady
--       nextSt = if outReady then nextStRaw else st
-- go st@(UpConverterState {..}) (Just (PacketStreamM2S{..}), PacketStreamS2M inReady)
--   = (nextSt, (PacketStreamS2M outReady, toPacketStream st))
--     where
--       inLast = isJust _last
--       -- We smear an abort over the entire rest of the packet
--       -- so the next abort is set:
--       --  - If fragment we are potentially flushing was not the last and we were already aborting;
--       --  - or if the incoming fragment is aborted
--       nextAbort = (isNothing _ucLastIdx && _ucAborted) || _abort
--       -- If we are not flushing we can accept data to be stored in _ucBuf,
--       -- but when we are flushing we can only accept if the current
--       -- output fragment is accepted by the sink
--       outReady = not _ucFlush || inReady
--       bufFull = _ucIdx == maxBound
--       nextBuf = replace _ucIdx (head _data) _ucBuf

--       nextFlush = inLast || bufFull
--       nextIdx = if nextFlush then 0 else _ucIdx + 1

--       nextStRaw = UpConverterState
--                     { _ucBuf =  nextBuf
--                     , _ucIdx = nextIdx
--                     , _ucFlush = nextFlush
--                     , _ucAborted = nextAbort
--                     , _ucLastIdx = toMaybe inLast _ucIdx
--                     }
--       nextSt = if outReady then nextStRaw else st

{-
    upConv stream = reverse (go [] [] 0 stream)
    go out acc accSize [] = acc:out
    go out acc accSize (inp:inps) = 
      case inp of
        Nothing -> go acc accSize packets 
        Just (str@(PacketStreamM2S {..})) -> case _last of
          True  -> go (acc:out) [] 0 inps
          False -> case accSize of
            4 -> go (acc:out) acc accSize
            _ -> go out acc (accSize + 1) inps
-}
  -- This is used to generate
    genPackets =
      PacketStreamM2S <$>
      (genVec Gen.enumBounded) <*>
      (Gen.maybe Gen.enumBounded) <*>
      Gen.enumBounded <*>
      Gen.enumBounded

tests :: TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1000))
  $(testGroupGenerator)
