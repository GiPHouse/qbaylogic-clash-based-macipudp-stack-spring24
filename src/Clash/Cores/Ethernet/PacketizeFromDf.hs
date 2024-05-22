{-|
Module      : Clash.Cores.Arp.PacketizeFromDf
Description : Packetize headers from Df inputs.
-}

{-# language FlexibleContexts #-}
{-# language RecordWildCards #-}

module Clash.Cores.Ethernet.PacketizeFromDf
  (packetizeFromDfC) where

import Clash.Prelude

import Protocols
import Protocols.Df

import Clash.Cores.Ethernet.PacketStream
import Clash.Cores.Ethernet.Util

import Data.Constraint.Deferrable ( (:~:)(Refl) )


-- | If dataWidth >= headerBytes, we don't need a buffer because we can immediately send
--   the fragment. Else, we need a buffer that stores the headerBytes minus the size
--   of the fragment we send out immediately.
type HeaderBufSize headerBytes dataWidth = dataWidth `Max` headerBytes - dataWidth

data DfPacketizerState (metaOut :: Type) (headerBytes :: Nat) (dataWidth :: Nat)
  = Idle
  | Insert {
      _counter :: Index (headerBytes `DivRU` dataWidth - 1),
      _hdrBuf :: Vec (HeaderBufSize headerBytes dataWidth) (BitVector 8)
    }
    deriving (Generic, NFDataX, Show, ShowX)

defaultByte :: BitVector 8
defaultByte = 0x00

packetizeFromDfT
  :: forall (dataWidth :: Nat)
            (a :: Type)
            (metaOut :: Type)
            (header :: Type)
            (headerBytes :: Nat)
   . NFDataX metaOut
  => BitPack header
  => BitSize header ~ headerBytes * 8
  => KnownNat headerBytes
  => KnownNat dataWidth
  => 1 <= dataWidth
  => 1 <= headerBytes `DivRU` dataWidth
  => dataWidth `Min` headerBytes <= dataWidth
  => dataWidth `Max` headerBytes - dataWidth + dataWidth `Min` headerBytes ~ headerBytes
  => (a -> metaOut)
  -- ^ function that transforms the Df input to the output metadata.
  -> (a -> header)
  -- ^ function that transforms the Df input to the header that will be packetized.
  -> DfPacketizerState metaOut headerBytes dataWidth
  -> (Data a, PacketStreamS2M)
  -> ( DfPacketizerState metaOut headerBytes dataWidth
     , (Ack, Maybe (PacketStreamM2S dataWidth metaOut)))
packetizeFromDfT toMetaOut toHeader Idle (Data dataIn, bwdIn) = (nextStOut, (bwdOut, Just outPkt))
  where
    rotatedHdr = rotateRightS (bitCoerce (toHeader dataIn)) (SNat @(HeaderBufSize headerBytes dataWidth))
    (hdrBuf, dataOut) = splitAt (SNat @(HeaderBufSize headerBytes dataWidth)) rotatedHdr
    dataOutPadded = dataOut ++ repeat @(dataWidth - dataWidth `Min` headerBytes) defaultByte
    outPkt = PacketStreamM2S dataOutPadded newLast (toMetaOut dataIn) False

    (nextSt, bwdOut, newLast) = case compareSNat (SNat @headerBytes) (SNat @dataWidth) of
      SNatLE -> (Idle, Ack (_ready bwdIn), Just l)
        where
          l = case compareSNat (SNat @(headerBytes `Mod` dataWidth)) d0 of
            SNatLE -> natToNum @(dataWidth - 1)
            SNatGT -> natToNum @(headerBytes `Mod` dataWidth - 1)
      SNatGT -> (Insert 0 hdrBuf, Ack False, Nothing)

    nextStOut = if _ready bwdIn then nextSt else Idle

-- fwdIn is always Data in this state, because we assert backpressure in Idle before we go here
-- Thus, we don't need to store the metadata in the state.
packetizeFromDfT toMetaOut _ st@Insert{..} (Data dataIn, bwdIn) = (nextStOut, (bwdOut, Just outPkt))
  where
    (dataOut, newHdrBuf) = splitAt (SNat @dataWidth) (_hdrBuf ++ repeat @dataWidth defaultByte)
    outPkt = PacketStreamM2S dataOut newLast (toMetaOut dataIn) False

    newLast = toMaybe (_counter == maxBound) $ case compareSNat (SNat @(headerBytes `Mod` dataWidth)) d0 of
      SNatLE -> natToNum @(dataWidth - 1)
      SNatGT -> natToNum @(headerBytes `Mod` dataWidth - 1)

    bwdOut = Ack (_ready bwdIn && _counter == maxBound)
    nextSt = if _counter == maxBound then Idle else Insert (succ _counter) newHdrBuf
    nextStOut = if _ready bwdIn then nextSt else st

packetizeFromDfT _ _ s (NoData, bwdIn) = (s, (Ack (_ready bwdIn), Nothing))

-- | Starts a packet stream upon receiving some data.
--   The bytes to be packetized and the output metadata
--   are specified by the input functions.
packetizeFromDfC
  :: forall (dom :: Domain)
            (dataWidth :: Nat)
            (a :: Type)
            (metaOut :: Type)
            (header :: Type)
            (headerBytes :: Nat)
   . HiddenClockResetEnable dom
  => NFDataX metaOut
  => BitPack header
  => BitSize header ~ headerBytes * 8
  => KnownNat headerBytes
  => KnownNat dataWidth
  => 1 <= dataWidth
  => (a -> metaOut)
  -- ^ Function that transforms the Df input to the output metadata.
  -> (a -> header)
  -- ^ Function that transforms the Df input to the header that will be packetized.
  -> Circuit (Df dom a) (PacketStream dom dataWidth metaOut)
packetizeFromDfC toMetaOut toHeader = fromSignals ckt
  where
    maxMinProof = sameNat (SNat @headerBytes) (SNat @(dataWidth `Max` headerBytes - dataWidth + dataWidth `Min` headerBytes))
    minProof = compareSNat (SNat @(dataWidth `Min` headerBytes)) (SNat @dataWidth)
    divRuProof = compareSNat d1 (SNat @(headerBytes `DivRU` dataWidth))

    ckt = case (maxMinProof, minProof, divRuProof) of
      (Just Refl, SNatLE, SNatLE) -> mealyB (packetizeFromDfT toMetaOut toHeader) Idle
      _ -> errorX "packetizeFromDfC: Absurd, Report this to the Clash compiler team: https://github.com/clash-lang/clash-compiler/issues"
