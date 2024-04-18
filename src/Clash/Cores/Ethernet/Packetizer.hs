module Clash.Cores.Ethernet.Packetizer where

import Clash.Prelude

import Protocols

import Clash.Cores.Ethernet.PacketStream

type ForwardBufSize (headerBytes :: Nat) (dataWidth :: Nat)
  = Mod headerBytes dataWidth

data PacketizerState (metaOut :: Type) (headerBytes :: Nat)  (dataWidth :: Nat)
  = Insert { _counter :: Index (DivRU headerBytes dataWidth) }
  | Forward {
      _metaOut :: metaOut,
       _fwdBuf :: Vec (ForwardBufSize headerBytes dataWidth) (BitVector 8)
    }
  | LastForward
      { _lastFragment :: PacketStreamM2S dataWidth metaOut }
    deriving (Generic)

type PacketizerCt (headerBytes :: Nat) (dataWidth :: Nat)
  = ( 1 <= DivRU headerBytes dataWidth
    , Mod headerBytes dataWidth <= dataWidth
    , KnownNat dataWidth
    , 1 <= dataWidth
    , KnownNat headerBytes
    )

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
packetizerT = errorX "not implemented"
{-packetizerT toMetaData toHeader Insert {_counter = i} (Just inp, bwdIn) = out
  where
    header = toHeader (_meta inp)
    out = if i == maxBound
          then (Forward {}, (PacketStreamS2M False, fwdOut1))
          else (Insert {_counter = i+1}, (PacketStreamS2M True, fwdOut2))
    fwdOut = errorX ""

packetizerT _ _ s (_, bwdIn) = (s, (bwdIn, Nothing))-}

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
  => (metaIn -> metaOut)
  -- ^ Metadata transformer
  -> (metaIn -> header)
  -- ^ metaData to header that will be packetized transformer
  -> Circuit (PacketStream dom dataWidth metaIn) (PacketStream dom dataWidth metaOut)
packetizerC _toMeta _toHeader = errorX "not implemented"
