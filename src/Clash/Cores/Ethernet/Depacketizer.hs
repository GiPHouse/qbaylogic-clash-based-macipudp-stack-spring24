{-# language AllowAmbiguousTypes #-}
{-# language FlexibleContexts #-}
{-# language RecordWildCards #-}

module Clash.Cores.Ethernet.Depacketizer
  (depacketizerC) where
import Data.Constraint ( Dict(..), (:-)(..) )
import Data.Maybe
import Unsafe.Coerce ( unsafeCoerce )

import Clash.Prelude

import Protocols

import Clash.Cores.Ethernet.PacketStream

-- When we are parsing we need to parse a total of @headerBytes@ bytes
-- and we receive upto @dataWidth@ bytes every cycle.
-- This means we size a shift register that contains @headerBytes@ bytes
-- in @dataWidth@ increments minus the bytes we still receive in the current cycle.
--
-- Ex. We parse a header of 19 bytes and our @dataWidth@ is 4 bytes.
--
-- That means:
--   full dataWidth increments we need: 19 `DivRU` 4 = 5
--   Now bring it back up to @dataWidth@ increments: 5 * 4 = 20
--   But we only need to store one less in the shift register: 20 - 4 = 16
type ParseBufSize (headerBytes :: Nat) (dataWidth :: Nat)
  = dataWidth * (headerBytes `DivRU` dataWidth) - dataWidth

-- Since the header might be unaligned compared to the datawidth
-- we need to store a partial fragment when forwarding.
-- The fragment we need to store depends on our "unalignedness".
--
-- Ex. We parse a header of 17 bytes and our @dataWidth@ is 4 bytes.
-- That means at the end of the header we can have upto 3 bytes left
-- in the fragment which we may need to forward.
type ForwardBufSize (headerBytes :: Nat) (dataWidth :: Nat)
  = (dataWidth - (headerBytes `Mod` dataWidth)) `Mod` dataWidth

-- Postulates that our parseBuffer size with an additional @dataWidth@
-- bytes is the sames as @headerBytes@ plus the forward buffer size.
--
-- If this is modified in the future the test named prop_equivalentBufSizes
-- In Test.Cores.Ethernet.Depacketizer must be changed.
equivalentBufSizes ::
  forall (headerBytes :: Nat) (dataWidth :: Nat).
  (1 <= dataWidth)
    :- ( ParseBufSize headerBytes dataWidth + dataWidth
         ~
         headerBytes + ForwardBufSize headerBytes dataWidth
       )
equivalentBufSizes = unsafeCoerce (Dict :: Dict (0 ~ 0))

type DepacketizerCt (headerBytes :: Nat) (dataWidth :: Nat)
  = ( 1 <= headerBytes `DivRU` dataWidth
    , headerBytes `Mod` dataWidth <= dataWidth
    , KnownNat dataWidth
    , 1 <= dataWidth
    , KnownNat headerBytes
    )

data DepacketizerState (metaOut :: Type) (headerBytes :: Nat)  (dataWidth :: Nat)
  = Parse
      { _aborted :: Bool
      , _parseBuf :: Vec (ParseBufSize headerBytes dataWidth) (BitVector 8)
      , _counter :: Index (headerBytes `DivRU` dataWidth)
      }
  | Forward
      { _aborted :: Bool
      , _metaOut :: metaOut
      , _fwdBuf :: Vec (ForwardBufSize headerBytes dataWidth) (BitVector 8)
      }
  | LastForward
      { _lastFragment :: PacketStreamM2S dataWidth metaOut }
  deriving (Generic)

deriving instance (Show metaOut , DepacketizerCt headerBytes dataWidth)
  => Show (DepacketizerState metaOut headerBytes dataWidth)

deriving instance (ShowX metaOut , DepacketizerCt headerBytes dataWidth)
  => ShowX (DepacketizerState metaOut headerBytes dataWidth)

deriving instance (NFDataX metaOut , DepacketizerCt headerBytes dataWidth)
  => NFDataX (DepacketizerState metaOut headerBytes dataWidth)

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
  => (header -> metaIn -> metaOut)
  -> DepacketizerState metaOut headerBytes dataWidth
  -> (Maybe (PacketStreamM2S dataWidth metaIn), PacketStreamS2M)
  -> ( DepacketizerState metaOut headerBytes dataWidth
     , (PacketStreamS2M, Maybe (PacketStreamM2S dataWidth metaOut))
     )
depacketizerT toMetaOut st@Parse {..} (Just pkt@PacketStreamM2S {..}, bwdIn) = (nextStOut, (PacketStreamS2M readyOut, fwdOut))
  where
    nextAborted = _aborted || _abort
    combined = _parseBuf ++ _data
    headerBytes :: Vec headerBytes (BitVector 8)
    (headerBytes, forwardBytes)
      = case equivalentBufSizes @headerBytes @dataWidth of Sub Dict -> splitAt (SNat @headerBytes) combined

    metaOut = toMetaOut (bitCoerce headerBytes) _meta
    dataOut = forwardBytes ++ (repeat defaultByte :: Vec (dataWidth - ForwardBufSize headerBytes dataWidth) (BitVector 8))

    prematureEnd idx
      = case compareSNat (SNat @(headerBytes `Mod` dataWidth)) d0 of
          SNatLE -> True
          SNatGT -> idx < (natToNum @(headerBytes `Mod` dataWidth))

    outPkt = pkt { _abort = nextAborted
                 , _data = dataOut
                 , _meta = metaOut
                 , _last = fmap (\i -> i - natToNum @(headerBytes `Mod` dataWidth)) _last
                 }

    (nextSt, fwdOut)
      = case (_counter == 0, _last) of
          (False, Nothing)
            -> (Parse nextAborted (drop (SNat @dataWidth) combined) (pred _counter), Nothing)
          (c, Just idx) | not c || prematureEnd idx
            -> ( Parse False (repeat defaultByte) maxBound, Nothing)
          (True, Just _)
            -> ( Parse False (repeat defaultByte) maxBound, Just outPkt)
          (True, Nothing)
            -> ( Forward nextAborted metaOut forwardBytes, Nothing)
          _ -> errorX "depacketizerT Parse: Absurd, Report this to the Clash compiler team: https://github.com/clash-lang/clash-compiler/issues"

    readyOut = isNothing fwdOut || _ready bwdIn
    nextStOut = if readyOut then nextSt else st

depacketizerT _ st@Parse{} (Nothing, bwdIn) = (st, (bwdIn, Nothing))

depacketizerT _ st@Forward {..} (Just pkt@PacketStreamM2S{..}, bwdIn) = (nextStOut, (bwdIn, Just outPkt))
  where
    nextAborted = _aborted || _abort
    combined :: Vec (ForwardBufSize headerBytes dataWidth + dataWidth) (BitVector 8)
    combined = _fwdBuf ++ _data
    dataOut :: Vec dataWidth (BitVector 8)
    nextFwdBuf :: Vec (ForwardBufSize headerBytes dataWidth) (BitVector 8)
    (dataOut, nextFwdBuf) = splitAt (SNat @dataWidth) combined

    dataLast = nextFwdBuf ++ (repeat defaultByte :: Vec (dataWidth - ForwardBufSize headerBytes dataWidth) (BitVector 8))

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
                 , _meta = _metaOut
                 , _last = either Just (const Nothing) =<< newLast
                 }

    nextSt = case newLast of
               Nothing -> Forward nextAborted _metaOut nextFwdBuf
               Just (Left _) -> Parse False (repeat defaultByte) maxBound
               Just (Right idx) -> LastForward $ PacketStreamM2S dataLast (Just idx) _metaOut nextAborted

    nextStOut = if _ready bwdIn then nextSt else st
depacketizerT _ st@Forward{} (Nothing, bwdIn) = (st, (bwdIn, Nothing))

depacketizerT _ st@LastForward{..} (_, bwdIn) = (nextStOut, (PacketStreamS2M False, Just _lastFragment))
  where
    nextStOut = if _ready bwdIn then Parse False (repeat defaultByte) maxBound else st

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
  -> Circuit (PacketStream dom dataWidth metaIn) (PacketStream dom dataWidth metaOut)
depacketizerC toMetaOut = forceResetSanity |> fromSignals outCircuit
  where
    divProof = compareSNat d1 (SNat @(headerBytes `DivRU` dataWidth))
    modProof = compareSNat (SNat @(headerBytes `Mod` dataWidth)) (SNat @dataWidth)

    outCircuit
      = case (divProof, modProof) of
          (SNatLE, SNatLE) -> case compareSNat (SNat @(ForwardBufSize headerBytes dataWidth)) (SNat @dataWidth) of
            SNatLE -> mealyB (depacketizerT @headerBytes toMetaOut) (Parse False (repeat undefined) maxBound)
            _ -> errorX "depacketizer1: Absurd, Report this to the Clash compiler team: https://github.com/clash-lang/clash-compiler/issues"
          _ -> errorX "depacketizer0: Absurd, Report this to the Clash compiler team: https://github.com/clash-lang/clash-compiler/issues"
