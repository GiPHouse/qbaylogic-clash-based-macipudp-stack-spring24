{-# language AllowAmbiguousTypes #-}
{-# language FlexibleContexts #-}
{-# language RecordWildCards #-}

{-|
Module      : Clash.Cores.Ethernet.Depacketizer
Description : Generic depacketizer for stripping headers from beginning of packets
-}
module Clash.Cores.Ethernet.Depacketizer
  (depacketizerC) where
import Data.Maybe

import Clash.Prelude
import Clash.Sized.Vector.Extra

import Protocols

import Clash.Cores.Ethernet.PacketStream

-- Since the header might be unaligned compared to the datawidth
-- we need to store a partial fragment when forwarding.
-- The fragment we need to store depends on our "unalignedness".
--
-- Ex. We parse a header of 17 bytes and our @dataWidth@ is 4 bytes.
-- That means at the end of the header we can have upto 3 bytes left
-- in the fragment which we may need to forward.
type ForwardBufSize (headerBytes :: Nat) (dataWidth :: Nat)
  = (dataWidth - (headerBytes `Mod` dataWidth)) `Mod` dataWidth

type DepacketizerCt (headerBytes :: Nat) (dataWidth :: Nat)
  = ( headerBytes `Mod` dataWidth <= dataWidth
    , KnownNat dataWidth
    , 1 <= dataWidth
    , KnownNat headerBytes
    )

data DepacketizerState (headerBytes :: Nat)  (dataWidth :: Nat)
  = Parse
      { _aborted :: Bool
      , _parseBuf :: Vec (dataWidth * headerBytes `DivRU` dataWidth) (BitVector 8)
      , _fwdBuf :: Vec (ForwardBufSize headerBytes dataWidth) (BitVector 8)
      , _counter :: Index (headerBytes `DivRU` dataWidth)
      }
  | Forward
      { _aborted :: Bool
      , _parseBuf :: Vec (dataWidth * headerBytes `DivRU` dataWidth) (BitVector 8)
      , _fwdBuf :: Vec (ForwardBufSize headerBytes dataWidth) (BitVector 8)
      , _counter :: Index (headerBytes `DivRU` dataWidth)
      }
  | LastForward
      { _aborted :: Bool
      , _parseBuf :: Vec (dataWidth * headerBytes `DivRU` dataWidth) (BitVector 8)
      , _fwdBuf :: Vec (ForwardBufSize headerBytes dataWidth) (BitVector 8)
      , _counter :: Index (headerBytes `DivRU` dataWidth)
      , _last_idx :: Index dataWidth
      }
  deriving (Show, ShowX, Generic)

deriving instance (DepacketizerCt headerBytes dataWidth)
  => NFDataX (DepacketizerState headerBytes dataWidth)

defaultByte :: BitVector 8
defaultByte = 0

depacketizerT
  :: forall (headerBytes :: Nat)
            (dataWidth :: Nat)
            (header :: Type)
            (metaIn :: Type)
            (metaOut :: Type)
   . BitSize header ~ headerBytes * 8
  => BitPack header
  => DepacketizerCt headerBytes dataWidth
  => ForwardBufSize headerBytes dataWidth <= dataWidth
  => headerBytes <= dataWidth * headerBytes `DivRU` dataWidth
  => (header -> metaIn -> metaOut)
  -> DepacketizerState headerBytes dataWidth
  -> (Maybe (PacketStreamM2S dataWidth metaIn), PacketStreamS2M)
  -> ( DepacketizerState headerBytes dataWidth
     , (PacketStreamS2M, Maybe (PacketStreamM2S dataWidth metaOut))
     )
depacketizerT _ Parse {..} (Just PacketStreamM2S {..}, _) = (nextSt, (PacketStreamS2M outReady, Nothing))
  where
    nextAborted = _aborted || _abort
    nextParseBuf = fst $ shiftInAtN _parseBuf _data
    fwdBuf :: Vec (ForwardBufSize headerBytes dataWidth) (BitVector 8)
    fwdBuf = dropLe (SNat @(dataWidth - ForwardBufSize headerBytes dataWidth)) _data

    prematureEnd idx
      = case compareSNat (SNat @(headerBytes `Mod` dataWidth)) d0 of
          SNatLE -> True
          SNatGT -> idx < (natToNum @(headerBytes `Mod` dataWidth))

    nextCounter = pred _counter

    nextSt
      = case (_counter == 0, _last) of
          (False, Nothing)
            -> Parse nextAborted nextParseBuf fwdBuf nextCounter
          (done, Just idx) | not done || prematureEnd idx
            -> Parse False nextParseBuf fwdBuf maxBound
          (True, Just idx)
            -> LastForward nextAborted nextParseBuf fwdBuf nextCounter (idx - natToNum @(headerBytes `Mod` dataWidth))
          (True, Nothing)
            -> Forward nextAborted nextParseBuf fwdBuf nextCounter
          _ -> errorX "depacketizerT Parse: Absurd, Report this to the Clash compiler team: https://github.com/clash-lang/clash-compiler/issues"

    outReady | LastForward {} <- nextSt = False
             | otherwise = True

depacketizerT _ st@Parse{} (Nothing, bwdIn) = (st, (bwdIn, Nothing))

depacketizerT toMetaOut st@Forward {..} (Just pkt@PacketStreamM2S{..}, bwdIn) = (nextStOut, (PacketStreamS2M outReady, Just outPkt))
  where
    nextAborted = _aborted || _abort
    dataOut :: Vec dataWidth (BitVector 8)
    nextFwdBuf :: Vec (ForwardBufSize headerBytes dataWidth) (BitVector 8)
    (dataOut, nextFwdBuf) = splitAt (SNat @dataWidth) $ _fwdBuf ++ _data

    adjustLast :: Index dataWidth -> Either (Index dataWidth) (Index dataWidth)
    adjustLast idx = if outputNow then Left nowIdx else Right nextIdx
      where
        outputNow = case compareSNat (SNat @(headerBytes `Mod` dataWidth)) d0 of
          SNatLE -> True
          SNatGT -> idx < natToNum @(headerBytes `Mod` dataWidth)
        nowIdx = idx + natToNum @(ForwardBufSize headerBytes dataWidth)
        nextIdx = idx - natToNum @(headerBytes `Mod` dataWidth)

    newLast = fmap adjustLast _last
    outPkt = pkt { _abort = nextAborted
                 , _data = dataOut
                 , _meta = toMetaOut (bitCoerce $ takeLe (SNat @headerBytes) _parseBuf) _meta
                 , _last = either Just (const Nothing) =<< newLast
                 }

    nextSt = case newLast of
               Nothing -> Forward nextAborted _parseBuf nextFwdBuf _counter
               Just (Left _) -> Parse False _parseBuf nextFwdBuf maxBound
               Just (Right idx) -> LastForward nextAborted _parseBuf nextFwdBuf _counter idx
    nextStOut = if _ready bwdIn then nextSt else st

    outReady | LastForward {} <- nextSt = False
             | otherwise = _ready bwdIn

depacketizerT _ st@Forward{} (Nothing, bwdIn) = (st, (bwdIn, Nothing))

depacketizerT toMetaOut st@LastForward{..} (fwdIn, bwdIn) = (nextStOut, (bwdIn, Just outPkt))
  where
    -- We can only get in this state if the previous clock cycle we received a fwdIn
    -- which was also the last fragment
    inPkt = fromJustX fwdIn
    outPkt = PacketStreamM2S
               { _abort = _aborted || _abort inPkt
               , _data = _fwdBuf ++ repeat @(dataWidth - ForwardBufSize headerBytes dataWidth) defaultByte
               , _meta = toMetaOut (bitCoerce $ takeLe (SNat @headerBytes) _parseBuf) (_meta inPkt)
               , _last = Just $ fromJustX (_last inPkt) - natToNum @(headerBytes `Mod` dataWidth)
               }
    nextStOut = if _ready bwdIn then Parse False _parseBuf _fwdBuf maxBound else st

-- | Reads bytes at the start of each packet into metadata.
depacketizerC
  :: forall (dom :: Domain)
            (dataWidth :: Nat)
            (metaIn :: Type)
            (metaOut :: Type)
            (header :: Type)
            (headerBytes :: Nat) .
  ( HiddenClockResetEnable dom
  , NFDataX metaOut
  , BitPack header
  , BitSize header ~ headerBytes * 8
  , KnownNat headerBytes
  , 1 <= dataWidth
  , KnownNat dataWidth)
  => (header -> metaIn -> metaOut)
  -- ^ Used to compute final metadata of outgoing packets from header and incoming metadata
  -> Circuit (PacketStream dom dataWidth metaIn) (PacketStream dom dataWidth metaOut)
depacketizerC toMetaOut = forceResetSanity |> fromSignals outCircuit
  where
    modProof = compareSNat (SNat @(headerBytes `Mod` dataWidth)) (SNat @dataWidth)
    divProof = compareSNat (SNat @headerBytes) (SNat @(dataWidth * headerBytes `DivRU` dataWidth))

    outCircuit
      = case (modProof, divProof) of
          (SNatLE, SNatLE) -> case compareSNat (SNat @(ForwardBufSize headerBytes dataWidth)) (SNat @dataWidth) of
            SNatLE -> mealyB (depacketizerT @headerBytes toMetaOut) (Parse False (repeat undefined) (repeat undefined) maxBound)
            _ -> errorX "depacketizer1: Absurd, Report this to the Clash compiler team: https://github.com/clash-lang/clash-compiler/issues"
          _ -> errorX "depacketizer0: Absurd, Report this to the Clash compiler team: https://github.com/clash-lang/clash-compiler/issues"
