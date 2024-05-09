{-# language FlexibleContexts #-}

{-|
Module      : Clash.Cores.Ethernet.PacketBuffer
Description : FIFO-like circuit for packet streams
-}
module Clash.Cores.Ethernet.PacketBuffer
    ( packetBufferC
    , overflowDropPacketBufferC
    ) where

import Clash.Cores.Ethernet.PacketStream
import Clash.Cores.Ethernet.Util
import Clash.Prelude
import Data.Maybe

import Protocols ( Circuit(..), fromSignals, (|>) )
import Protocols.Internal ( CSignal(..) )

type PacketStreamContent(dataWidth :: Nat) (metaType :: Type) = (Vec dataWidth (BitVector 8), Maybe (Index dataWidth))

toPacketStreamContent :: PacketStreamM2S dataWidth metaType -> PacketStreamContent dataWidth metaType
toPacketStreamContent PacketStreamM2S{ _data=d, _last=l, _meta=_, _abort=_ } = (d,l)

toPacketStreamM2S :: PacketStreamContent dataWidth metaType -> metaType -> PacketStreamM2S dataWidth metaType
toPacketStreamM2S (d, l) m = PacketStreamM2S d l m False

packetBuffer
  :: forall (dom :: Domain) (dataWidth :: Nat) (metaType :: Type) (contentSizeBits :: Nat) (metaSizeBits :: Nat) .
  HiddenClockResetEnable dom
  => KnownNat dataWidth
  => 1 <= contentSizeBits => 1 <= metaSizeBits
  => NFDataX metaType
  => SNat contentSizeBits
  -- ^ Depth of the content of the packet buffer, this is equal to 2^contentSizeBits
  -> SNat metaSizeBits
  -- ^ Depth of the content of the meta buffer, this is equal to 2^metaSizeBits
  -> ( Signal dom (Maybe (PacketStreamM2S dataWidth metaType))
     , Signal dom PacketStreamS2M
     )
  -- ^ Input packetStream
  -> (  Signal dom PacketStreamS2M
     , Signal dom (Maybe (PacketStreamM2S dataWidth metaType))
     )
  -- ^ Output CSignal s
packetBuffer SNat SNat (fwdIn, bwdIn) = (PacketStreamS2M . not <$> fullBuffer, fwdOut)
  where
    fwdOut = toMaybe <$> (not <$> emptyBuffer) <*> (toPacketStreamM2S <$> ramContent <*> ramMeta)

    -- The backing ram
    ramContent = blockRam1 NoClearOnReset (SNat @(2 ^ contentSizeBits)) (errorX "initial block ram content") cReadAddr' writeCommand
    ramMeta = blockRam1 NoClearOnReset (SNat @(2 ^ metaSizeBits)) (errorX "initial block ram meta content") mReadAddr' mWriteCommand

      -- The write commands to the RAM
    writeCommand = toMaybe <$> writeEnable <*> bundle(cWordAddr, toPacketStreamContent . fromJustX <$> fwdIn)
    mWriteCommand = toMaybe <$> nextPacketIn <*> bundle(mWriteAddr, _meta . fromJustX <$> fwdIn)

    -- Addresses for the content data
    cWordAddr, cPacketAddr, cReadAddr :: Signal dom (Unsigned contentSizeBits)
    cWordAddr = register 0 $ mux dropping' cPacketAddr $ mux writeEnable (cWordAddr + 1) cWordAddr
    cPacketAddr = register 0 $ mux nextPacketIn (cWordAddr + 1) cPacketAddr
    cReadAddr' = mux readEnable (cReadAddr + 1) cReadAddr
    cReadAddr = register 0 cReadAddr'

    -- Addresses for the meta data
    mWriteAddr, mReadAddr :: Signal dom (Unsigned metaSizeBits)
    mWriteAddr = register 0 mWriteAddr'
    mWriteAddr' = mux nextPacketIn (mWriteAddr + 1) mWriteAddr

    mReadAddr' = mux mReadEnable (mReadAddr + 1) mReadAddr
    mReadAddr = register 0 mReadAddr'
    -- only read the next value if we've outpustted the last word of a packet
    mReadEnable = lastWordOut .&&. readEnable

    -- Registers : status
    dropping', dropping, emptyBuffer :: Signal dom Bool
    -- start dropping packet on abort
    dropping' = abortIn .||. dropping
    dropping = register False $ dropping' .&&. (not <$> lastWordIn)
    -- the buffer is empty if the metaBuffer is empty as the metabuffer only updates when a packet is complete
    emptyBuffer = register 0 mWriteAddr .==. mReadAddr

    -- Only write if there is space and we're not dropping
    writeEnable = writeRequest .&&. (not <$> fullBuffer) .&&. (not <$> dropping')
    -- Read when the word has been received
    readEnable = (not <$> emptyBuffer) .&&. (_ready <$> bwdIn)

    --The status signals
    fullBuffer = ((cWordAddr + 1) .==. cReadAddr) .||. ((mWriteAddr + 1) .==. mReadAddr)
    writeRequest = isJust <$> fwdIn
    lastWordIn = maybe False (isJust . _last) <$> fwdIn
    lastWordOut = maybe False (isJust . _last) <$> fwdOut
    abortIn = maybe False _abort <$> fwdIn
    nextPacketIn = lastWordIn .&&. writeEnable

-- | A circuit that sends an abort forward if there is backpressure from the forward circuit
abortOnBackPressure
  :: forall  (dom :: Domain) (dataWidth :: Nat) (metaType :: Type).
  HiddenClockResetEnable dom
  => KnownNat dataWidth
  => NFDataX metaType
  => ( CSignal dom (Maybe (PacketStreamM2S dataWidth metaType))
     , Signal dom PacketStreamS2M
     )
  -> ( CSignal dom ()
     , Signal dom (Maybe (PacketStreamM2S dataWidth metaType))
     )
  -- ^ Does not give backpressure, sends an abort forward instead
abortOnBackPressure (CSignal fwdInS, bwdInS) = (CSignal $ pure (), go <$> bundle (fwdInS, bwdInS))
  where
    go (fwdIn, bwdIn) = fmap (\pkt -> pkt { _abort = _abort pkt || not (_ready bwdIn) }) fwdIn

-- | Packet buffer, a circuit which stores words in a buffer until the packet is complete
-- once a packet is complete it will send the entire packet out at once without stalls.
-- If a word in a packet has _abort set to true, the packetBuffer will drop the entire packet.
-- If a packet is equal to or larger than 2^sizeBits-1, the packetBuffer will have a deadlock, this should be avoided!
packetBufferC
  :: forall  (dom :: Domain)  (dataWidth :: Nat) (metaType :: Type)
      (contentSizeBits :: Nat) (metaSizeBits :: Nat).
  HiddenClockResetEnable dom
    => KnownNat dataWidth
    => 1 <= contentSizeBits => 1 <= metaSizeBits
    => NFDataX metaType
    => SNat contentSizeBits
    -- ^ Depth of the content of the packet buffer, this is equal to 2^contentSizeBits
    -> SNat metaSizeBits
    -- ^ Depth for the meta of the packet buffer, this is equal to 2^metaSizeBits.
    -- This can usually be smaller than contentSizeBits as for every packet we only need a single meta entry, while we usually have many words.
    -> Circuit (PacketStream dom dataWidth metaType) (PacketStream dom dataWidth metaType)
packetBufferC cSizeBits mSizeBits = forceResetSanity |> fromSignals (packetBuffer cSizeBits mSizeBits)

-- | A packet buffer that drops packets when it is full, instead of giving backpressure, see packetBufferC for more detailed explanation
overflowDropPacketBufferC
  :: forall  (dom :: Domain)  (dataWidth :: Nat) (metaType :: Type)
      (contentSizeBits :: Nat) (metaSizeBits :: Nat).
  HiddenClockResetEnable dom
    => KnownNat dataWidth
    => 1 <= contentSizeBits => 1 <= metaSizeBits
    => NFDataX metaType
    => SNat contentSizeBits
    -> SNat metaSizeBits
    -> Circuit (CSignal dom (Maybe (PacketStreamM2S dataWidth metaType))) (PacketStream dom dataWidth metaType)
overflowDropPacketBufferC cSizeBits mSizeBits =  backPressureC |> fromSignals (packetBuffer cSizeBits mSizeBits)
  where
    backPressureC :: Circuit (CSignal dom (Maybe (PacketStreamM2S dataWidth metaType))) (PacketStream dom dataWidth metaType)
    backPressureC = fromSignals abortOnBackPressure
