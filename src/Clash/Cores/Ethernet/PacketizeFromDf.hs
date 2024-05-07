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
packetizeFromDfT toMetaOut toHeader Idle (Data dataIn, bwdIn) = (nextStOut, (bwdOut, Just outPkt))
  where
    hdrBuf = bitCoerce (toHeader dataIn) ++ repeat @dataWidth defaultByte
    (newHdrBuf, dataOut) = shiftOutFrom0 (SNat @dataWidth) hdrBuf
    outPkt = PacketStreamM2S dataOut newLast (toMetaOut dataIn) False

    (nextSt, bwdOut, newLast) = case compareSNat (SNat @headerBytes) (SNat @dataWidth) of
      SNatLE -> (Idle, Ack (_ready bwdIn), l)
        where
          l = Just $ case compareSNat (SNat @(headerBytes `Mod` dataWidth)) d0 of
            SNatLE -> natToNum @(dataWidth - 1)
            SNatGT -> natToNum @(headerBytes `Mod` dataWidth - 1)
      SNatGT -> (Insert 0 newHdrBuf, Ack False, Nothing)
    nextStOut = if _ready bwdIn then nextSt else Idle

-- fwdIn is always Data in this state, because we assert backpressure in Idle before we go here
-- Thus, we don't need to store the metadata in the state.
packetizeFromDfT toMetaOut _ st@Insert{..} (Data dataIn, bwdIn) = (nextStOut, (bwdOut, Just outPkt))
  where
    (newHdrBuf, dataOut) = shiftOutFrom0 (SNat @dataWidth) _hdrBuf
    outPkt = PacketStreamM2S dataOut newLast (toMetaOut dataIn) False

    newLast = toMaybe (_counter == maxBound) $ case compareSNat (SNat @(headerBytes `Mod` dataWidth)) d0 of
      SNatLE -> natToNum @(dataWidth - 1)
      SNatGT -> natToNum @(headerBytes `Mod` dataWidth - 1)

    bwdOut = Ack (_ready bwdIn && _counter == maxBound)
    nextSt = if _counter == maxBound then Idle else Insert (succ _counter) newHdrBuf
    nextStOut = if _ready bwdIn then nextSt else st

packetizeFromDfT _ _ s (NoData, bwdIn) = (s, (Ack (_ready bwdIn), Nothing))

-- | Starts a packet stream upon receiving some data.
--   The bytes to be packetized are specified by the input function that
--   transforms our input data with type `a` to type `header`.
packetizeFromDfC
  :: forall (dom :: Domain)
            (dataWidth :: Nat)
            (a :: Type)
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
  => (a -> metaOut)
  -- ^ input to metadata transformer function
  -> (a -> header)
  -- ^ input to header that will be packetized transformer function
  -> Circuit (Df dom a) (PacketStream dom dataWidth metaOut)
packetizeFromDfC toMetaOut toHeader = case compareSNat d1 (SNat @(headerBytes `DivRU` dataWidth)) of
  SNatLE -> fromSignals (mealyB (packetizeFromDfT toMetaOut toHeader) Idle)
  SNatGT -> errorX "packetizeFromDfC: Absurd, Report this to the Clash compiler team: https://github.com/clash-lang/clash-compiler/issues"
