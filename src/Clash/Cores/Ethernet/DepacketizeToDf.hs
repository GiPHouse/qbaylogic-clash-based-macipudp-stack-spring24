{-|
Module      : Clash.Cores.Arp.PacketizeFromDf
Description : Create Df headers from Df inputs.
-}

{-# language RecordWildCards #-}

module Clash.Cores.Ethernet.DepacketizeToDf
  (depacketizeToDfC) where

import Clash.Cores.Ethernet.PacketStream
import Clash.Prelude
import Clash.Sized.Vector.Extra

import Protocols
import Protocols.Df qualified as Df

import Data.Maybe


type ParseBufSize (headerBytes :: Nat) (dataWidth :: Nat)
  = dataWidth * headerBytes `DivRU` dataWidth

type DepacketizeToDfCt (headerBytes :: Nat) (dataWidth :: Nat)
  = ( 1 <= headerBytes `DivRU` dataWidth
    , headerBytes `Mod` dataWidth <= dataWidth
    , headerBytes <= ParseBufSize headerBytes dataWidth
    , KnownNat dataWidth
    , 1 <= dataWidth
    , KnownNat headerBytes
    )

data DfDepacketizerState (a :: Type) (headerBytes :: Nat) (dataWidth :: Nat)
  = Parse {
        _aborted :: Bool
      -- ^ whether any of the fragments parsed from the current packet were aborted.
      , _parseBuf :: Vec (ParseBufSize headerBytes dataWidth) (BitVector 8)
      -- ^ the accumulator for header bytes.
      , _counter :: Index (headerBytes `DivRU` dataWidth)
      -- ^ how many of the _parseBuf bytes are currently valid (accumulation count). We flush at counter == maxBound
    }
  | ConsumePadding {
        _aborted :: Bool
        -- ^ whether any of the fragments parsed from the current packet were aborted.
      , _parseBuf :: Vec (ParseBufSize headerBytes dataWidth) (BitVector 8)
    }
  deriving (Generic, Show, ShowX)

deriving instance (NFDataX a , DepacketizeToDfCt headerBytes dataWidth)
  => NFDataX (DfDepacketizerState a headerBytes dataWidth)

initialState :: forall (a :: Type) (headerBytes :: Nat) (dataWidth :: Nat)
   . KnownNat dataWidth
  => KnownNat headerBytes
  => 1 <= dataWidth
  => DfDepacketizerState a headerBytes dataWidth
initialState = Parse False (repeat undefined) maxBound

depacketizeToDfT
  :: forall (dom :: Domain)
            (dataWidth :: Nat)
            (a :: Type)
            (meta :: Type)
            (header :: Type)
            (headerBytes :: Nat)
   . HiddenClockResetEnable dom
  => NFDataX meta
  => BitPack header
  => BitSize header ~ headerBytes * 8
  => DepacketizeToDfCt headerBytes dataWidth
  => (header -> meta -> a)
  -> DfDepacketizerState a headerBytes dataWidth
  -> (Maybe (PacketStreamM2S dataWidth meta), Ack)
  -> (DfDepacketizerState a headerBytes dataWidth, (PacketStreamS2M, Df.Data a))
depacketizeToDfT toOut st@Parse {..} (Just (PacketStreamM2S {..}), Ack readyIn) = (nextStOut, (PacketStreamS2M readyOut, fwdOut))
  where
    nextAborted = _aborted || _abort
    nextParseBuf = fst (shiftInAtN _parseBuf _data)
    outDf = toOut (bitCoerce (takeLe (SNat @headerBytes) nextParseBuf)) _meta

    prematureEnd idx
      = case compareSNat (SNat @(headerBytes `Mod` dataWidth)) d0 of
          SNatLE -> idx < (natToNum @(dataWidth - 1))
          SNatGT -> idx < (natToNum @(headerBytes `Mod` dataWidth - 1))

    (nextSt, fwdOut)
      = case (_counter == 0, _last) of
          (False, Nothing)
            -> (Parse nextAborted nextParseBuf (pred _counter), Df.NoData)
          (c, Just idx) | not c || prematureEnd idx
            -> ( initialState, Df.NoData)
          (True, Just _)
            -> ( initialState, if nextAborted then Df.NoData else Df.Data outDf)
          (True, Nothing)
            -> ( ConsumePadding nextAborted nextParseBuf, Df.NoData)
          _ -> errorX "depacketizeToDfT Parse: Absurd, Report this to the Clash compiler team: https://github.com/clash-lang/clash-compiler/issues"

    readyOut = isNothing (Df.dataToMaybe fwdOut) || readyIn
    nextStOut = if readyOut then nextSt else st

depacketizeToDfT toOut st@ConsumePadding {..} (Just (PacketStreamM2S {..}), Ack readyIn) = (nextStOut, (PacketStreamS2M readyOut, fwdOut))
  where
    nextAborted = _aborted || _abort
    outDf = toOut (bitCoerce (takeLe (SNat @headerBytes) _parseBuf)) _meta

    (nextSt, fwdOut)
      = if isJust _last
        then (initialState, if nextAborted then Df.NoData else Df.Data outDf)
        else (st {_aborted=nextAborted}, Df.NoData)

    readyOut = isNothing (Df.dataToMaybe fwdOut) || readyIn
    nextStOut = if readyOut then nextSt else st

depacketizeToDfT _ st (Nothing, Ack ready) = (st, (PacketStreamS2M ready, Df.NoData))

-- | Reads bytes at the start of each packet into a dataflow.
-- Consumes the remainder of the packet and drops this. If a
-- packet ends sooner than the assumed length of the header,
-- `depacketizeToDfC` does not send out anything.
-- If any of the fragments in the packet has _abort set, it drops
-- the entire packet.
depacketizeToDfC
  :: forall (dom :: Domain)
            (dataWidth :: Nat)
            (a :: Type)
            (meta :: Type)
            (header :: Type)
            (headerBytes :: Nat)
   . HiddenClockResetEnable dom
  => NFDataX meta
  => NFDataX a
  => BitPack header
  => KnownNat headerBytes
  => KnownNat dataWidth
  => 1 <= dataWidth
  => BitSize header ~ headerBytes * 8
  => (header -> meta -> a)
  -- ^ function that transforms the given meta + parsed header to the output Df
  -> Circuit (PacketStream dom dataWidth meta) (Df dom a)
depacketizeToDfC toOut = forceResetSanity |> fromSignals outCircuit
  where
    divProof = compareSNat (SNat @headerBytes) (SNat @(dataWidth * headerBytes `DivRU` dataWidth))
    modProof = compareSNat (SNat @(headerBytes `Mod` dataWidth)) (SNat @dataWidth)

    outCircuit
      = case (divProof, modProof) of
          (SNatLE, SNatLE) -> case compareSNat d1 (SNat @(headerBytes `DivRU` dataWidth)) of
              SNatLE -> mealyB (depacketizeToDfT toOut) initialState
              _ -> errorX "depacketizeToDfC0: Absurd, Report this to the Clash compiler team: https://github.com/clash-lang/clash-compiler/issues"
          _ -> errorX "depacketizeToDfC1: Absurd, Report this to the Clash compiler team: https://github.com/clash-lang/clash-compiler/issues"
