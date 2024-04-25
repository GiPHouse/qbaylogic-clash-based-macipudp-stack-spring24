--{-# language AllowAmbiguousTypes #-}
{-# language FlexibleContexts #-}
{-# language RecordWildCards #-}

module Clash.Cores.Ethernet.Packetizer where

import Clash.Prelude

import Protocols

import Clash.Cores.Ethernet.PacketStream

import Data.Maybe ( isNothing )

type HeaderBufSize (headerBytes :: Nat) (dataWidth :: Nat)
  = headerBytes + dataWidth--dataWidth * (headerBytes `DivRU` dataWidth) - dataWidth

type ForwardBufSize (headerBytes :: Nat) (dataWidth :: Nat)
  = headerBytes `Mod` dataWidth

data PacketizerState (metaOut :: Type) (headerBytes :: Nat)  (dataWidth :: Nat)
  = Insert {
      _counter :: Index (headerBytes `DivRU` dataWidth),
      _hdrBuf :: Vec (HeaderBufSize headerBytes dataWidth) (BitVector 8)
    }
  | Forward {
      _metaOut :: metaOut,
      _fwdBuf :: Vec (ForwardBufSize headerBytes dataWidth) (BitVector 8)
    }
  | LastForward
      { _lastFragment :: PacketStreamM2S dataWidth metaOut }
    deriving (Generic)

deriving instance (Show metaOut , PacketizerCt headerBytes dataWidth)
  => Show (PacketizerState metaOut headerBytes dataWidth)

deriving instance (ShowX metaOut , PacketizerCt headerBytes dataWidth)
  => ShowX (PacketizerState metaOut headerBytes dataWidth)

deriving instance (NFDataX metaOut , PacketizerCt headerBytes dataWidth)
  => NFDataX (PacketizerState metaOut headerBytes dataWidth)

type PacketizerCt (headerBytes :: Nat) (dataWidth :: Nat)
  = ( 1 <= DivRU headerBytes dataWidth
    , Mod headerBytes dataWidth <= dataWidth
    , KnownNat dataWidth
    , 1 <= dataWidth
    , KnownNat headerBytes
    , Mod headerBytes dataWidth <= (dataWidth - 1)
    )

defaultByte :: BitVector 8
defaultByte = 0

packetizerT
  :: forall (headerBytes :: Nat)
            (dataWidth :: Nat)
            (header :: Type)
            (metaIn :: Type)
            (metaOut :: Type)
   . BitSize header ~ headerBytes * 8
  => BitPack header
  => PacketizerCt headerBytes dataWidth
  => ForwardBufSize headerBytes dataWidth <= dataWidth
  => (metaIn -> metaOut)
  -> (metaIn -> header)
  -> PacketizerState metaOut headerBytes dataWidth
  -> (Maybe (PacketStreamM2S dataWidth metaIn), PacketStreamS2M)
  -> ( PacketizerState metaOut headerBytes dataWidth
     , (PacketStreamS2M, Maybe (PacketStreamM2S dataWidth metaOut)))
packetizerT toMetaOut toHeader st@Insert {..} (Just pkt@PacketStreamM2S {..}, bwdIn) = (nextStOut, (PacketStreamS2M outReady, fwdOut))
  where
    --nextAborted = _aborted || _abort
    header :: Vec headerBytes (BitVector 8) = bitCoerce $ toHeader _meta
    combined = header ++ _data

    metaOut = toMetaOut _meta
    --resized :: Index (headerBytes + dataWidth) = resize _counter

    outReady = case compareSNat (SNat @(headerBytes `Mod` dataWidth)) d0 of
      SNatLE -> False--counter == maxBound
      SNatGT -> _counter == maxBound



    hdrBuf = if _counter == minBound then combined else _hdrBuf

    dataOut = take (SNat @dataWidth) hdrBuf
    newHdrBuf = rotateLeftS hdrBuf (SNat @dataWidth)


    adjustLast :: Index dataWidth -> Either (Index dataWidth) (Index dataWidth)
    adjustLast idx = if outputNow then Left nowIdx else Right nextIdx
      where
        outputNow = case compareSNat (SNat @(headerBytes `Mod` dataWidth)) d0 of
          SNatLE -> True
          SNatGT -> idx < natToNum @(headerBytes `Mod` dataWidth)
        nowIdx = idx + natToNum @(dataWidth - 1 - ForwardBufSize headerBytes dataWidth)
        nextIdx = idx - natToNum @(dataWidth - 1 - ForwardBufSize headerBytes dataWidth)

    outPkt = pkt { _abort = False--nextAborted
                 , _data = dataOut
                 , _meta = metaOut
                 , _last = Nothing
                 }

    forwardBytes :: Vec (ForwardBufSize headerBytes dataWidth) (BitVector 8)
    forwardBytes = take (SNat @(ForwardBufSize headerBytes dataWidth)) (rotateRightS _data (SNat @(ForwardBufSize headerBytes dataWidth)))

    (nextSt, fwdOut)
      = case (_counter == maxBound, _last) of
          (False, Nothing)
            -> (Insert (succ _counter) newHdrBuf, Just outPkt)
          (False, _)
            -> ( Insert (succ _counter) newHdrBuf, Just outPkt)
          (True, Just idx)
            -> (a, Just b)--( Insert minBound newHdrBuf, Just outPkt)
            where
              newLast = adjustLast idx
              (a, b) = case newLast of
                Left i -> case compareSNat (SNat @(headerBytes `Mod` dataWidth)) d0 of
                  SNatLE -> (Forward metaOut forwardBytes, outPkt)
                  SNatGT -> (Insert minBound (repeat 0x00), outPkt {_last = Just i})
                Right i -> (LastForward pkt {_last = Just i, _meta = metaOut}, outPkt)
          (True, Nothing)
            -> ( Forward metaOut forwardBytes, Just outPkt)

    nextStOut = if isNothing fwdOut || _ready bwdIn then nextSt else st

packetizerT _ _ st@Forward {..} (Just pkt@PacketStreamM2S{..}, bwdIn) = (nextStOut, (bwdIn, Just outPkt))
  where
    --nextAborted = _aborted || _abort
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
    outPkt = pkt { _abort = False--nextAborted
                 , _data = dataOut
                 , _meta = _metaOut
                 , _last = either Just (const Nothing) =<< newLast
                 }

    nextSt = case newLast of
               Nothing -> Forward _metaOut nextFwdBuf --nextAborted _metaOut nextFwdBuf
               Just (Left _) -> Insert minBound (repeat 0x00)--Parse False (repeat defaultByte) maxBound
               Just (Right idx) -> LastForward $ PacketStreamM2S dataLast (Just idx) _metaOut False--nextAborted

    nextStOut = if _ready bwdIn then nextSt else st
packetizerT _ _ st@Forward{} (Nothing, bwdIn) = (st, (bwdIn, Nothing))

packetizerT _ _ st@LastForward{..} (_, bwdIn) = (nextStOut, (PacketStreamS2M False, Just _lastFragment))
  where
    nextStOut = if _ready bwdIn then Insert minBound (repeat 0x00) else st

packetizerT _ _ s (_, bwdIn) = (s, (bwdIn, Nothing))

packetizerC
  :: forall (dom :: Domain)
            (dataWidth :: Nat)
            (metaIn :: Type)
            (metaOut :: Type)
            (header :: Type)
            (headerBytes :: Nat)
   . HiddenClockResetEnable dom
  => NFDataX metaOut
  => BitPack header
  => BitSize header ~ headerBytes * 8
  => KnownNat headerBytes
  => 1 <= dataWidth
  => KnownNat dataWidth
  => Mod headerBytes dataWidth <= (dataWidth - 1)
  => (metaIn -> metaOut)
  -- ^ Metadata transformer
  -> (metaIn -> header)
  -- ^ metaData to header that will be packetized transformer
  -> Circuit (PacketStream dom dataWidth metaIn) (PacketStream dom dataWidth metaOut)
packetizerC toMetaOut toHeader = forceResetSanity |> fromSignals outCircuit
  where
    divProof = compareSNat d1 (SNat @(headerBytes `DivRU` dataWidth))
    modProof = compareSNat (SNat @(headerBytes `Mod` dataWidth)) (SNat @dataWidth)

    outCircuit
      = case (divProof, modProof) of
          (SNatLE, SNatLE) -> case compareSNat (SNat @(ForwardBufSize headerBytes dataWidth)) (SNat @dataWidth) of
            SNatLE -> mealyB (packetizerT @headerBytes toMetaOut toHeader) (Insert minBound (repeat 0x00))
            _ -> errorX "depacketizer1: Absurd, Report this to the Clash compiler team: https://github.com/clash-lang/clash-compiler/issues"
          _ -> errorX "depacketizer0: Absurd, Report this to the Clash compiler team: https://github.com/clash-lang/clash-compiler/issues"