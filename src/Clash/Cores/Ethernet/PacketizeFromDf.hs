{-# language FlexibleContexts #-}
{-# language RecordWildCards #-}

{-|
Module      : Clash.Cores.Arp.PacketizeFromDf
Description : Packetize headers from Df inputs.
-}
module Clash.Cores.Ethernet.PacketizeFromDf
  (packetizeFromDfC) where

import Clash.Prelude

import Protocols
import Protocols.Df

import Clash.Cores.Ethernet.PacketStream
import Clash.Cores.Ethernet.Util


type HeaderBufSize headerBytes dataWidth = headerBytes + dataWidth

data PacketizerState (metaOut :: Type) (headerBytes :: Nat) (dataWidth :: Nat)
  = Idle
  | Insert {
      _counter :: Index (headerBytes `DivRU` dataWidth - 1),
      _hdrBuf :: Vec (HeaderBufSize headerBytes dataWidth) (BitVector 8),
      _metaOut :: metaOut
    }
    deriving (Generic, NFDataX, Show, ShowX)

defaultByte :: BitVector 8
defaultByte = 0x00

packetizeFromDfT
  :: forall (dataWidth :: Nat)
            (a :: Type)
            (metaOut :: Type)
            (header :: Type)
            (headerBytes :: Nat) .
  ( NFDataX metaOut
  , BitPack header
  , BitSize header ~ headerBytes * 8
  , KnownNat headerBytes
  , 1 <= headerBytes `DivRU` dataWidth
  , 1 <= dataWidth
  , KnownNat dataWidth)
  => (a -> metaOut)
  -- ^ input to metadata transformer function
  -> (a -> header)
  -- ^ input to header that will be packetized transformer function
  -> PacketizerState metaOut headerBytes dataWidth
  -> (Data a, PacketStreamS2M)
  -> ( PacketizerState metaOut headerBytes dataWidth
     , (Ack, Maybe (PacketStreamM2S dataWidth metaOut)))
packetizeFromDfT toMetaOut toHeader Idle (Data dataIn, bwdIn) = (nextStateOut, (bwdOut, fwdOut))
  where
    metaOut = toMetaOut dataIn
    hdrBuf = bitCoerce (toHeader dataIn) ++ repeat @dataWidth defaultByte
    (newHdrBuf, dataOut) = shiftOutFrom0 (SNat @dataWidth) hdrBuf

    newLast :: Index dataWidth = case compareSNat (SNat @(headerBytes `Mod` dataWidth)) d0 of
      SNatLE -> natToNum @(dataWidth - 1)
      SNatGT -> natToNum @(headerBytes `Mod` dataWidth - 1)

    fwdOut = Just PacketStreamM2S {
      _data = dataOut,
      _last = case compareSNat (SNat @headerBytes) (SNat @dataWidth) of
        SNatLE -> Just newLast
        SNatGT -> Nothing,
      _meta = metaOut,
      _abort = False
    }

    (nextState, bwdOut) = case compareSNat (SNat @headerBytes) (SNat @dataWidth) of
      SNatLE -> (Idle, Ack (_ready bwdIn && True))
      SNatGT -> (Insert 0 newHdrBuf metaOut, Ack False)
    nextStateOut = if _ready bwdIn then nextState else Idle

packetizeFromDfT _ _ st@Insert{..} (_, bwdIn) = (nextStateOut, (bwdOut, fwdOut))
  where
    (newHdrBuf, dataOut) = shiftOutFrom0 (SNat @dataWidth) _hdrBuf

    newLast = case compareSNat (SNat @(headerBytes `Mod` dataWidth)) d0 of
      SNatLE -> natToNum @(dataWidth - 1)
      SNatGT -> natToNum @(headerBytes `Mod` dataWidth - 1)

    fwdOut = Just PacketStreamM2S {
      _data = dataOut,
      _last = toMaybe (_counter == maxBound) newLast,
      _meta = _metaOut,
      _abort = False
    }

    bwdOut = Ack (_ready bwdIn && _counter == maxBound)
    nextState = if _counter == maxBound then Idle else Insert (_counter + 1) newHdrBuf _metaOut
    nextStateOut = if _ready bwdIn then nextState else st

packetizeFromDfT _ _ s (NoData, bwdIn) = (s, (Ack (_ready bwdIn), Nothing))

-- | Starts a packet stream upon receiving some data.
--   The bytes to be packetized are specified by the input function that
--   transforms our input data with type `a` to type `header`.
packetizeFromDfC
  :: forall (dom :: Domain)
            (dataWidth :: Nat)
            (metaOut :: Type)
            (header :: Type)
            (a :: Type)
            (headerBytes :: Nat) .
  ( HiddenClockResetEnable dom
  , NFDataX metaOut
  , BitPack header
  , BitSize header ~ headerBytes * 8
  , KnownNat headerBytes
  , 1 <= dataWidth
  , KnownNat dataWidth)
  => (a -> metaOut)
  -- ^ Metadata transformer function
  -> (a -> header)
  -- ^ Metadata to header that will be packetized transformer function
  -> Circuit (Df dom a) (PacketStream dom dataWidth metaOut)
packetizeFromDfC toMetaOut toHeader = case compareSNat d1 (SNat @(headerBytes `DivRU` dataWidth)) of
  SNatLE -> fromSignals (mealyB (packetizeFromDfT toMetaOut toHeader) Idle)
  SNatGT -> errorX "packetizer'0: Absurd, Report this to the Clash compiler team: https://github.com/clash-lang/clash-compiler/issues"
