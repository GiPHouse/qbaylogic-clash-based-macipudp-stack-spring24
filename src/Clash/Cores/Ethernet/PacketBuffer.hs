{-# language FlexibleContexts #-}

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

data PacketStreamContent(dataWidth :: Nat) (metaType :: Type)
  = PacketStreamContent {
  _cdata :: Vec dataWidth (BitVector 8),
  -- ^ The bytes to be transmitted
  _clast :: Maybe (Index dataWidth),
  -- ^ If Nothing, we are not yet at the last byte, otherwise signifies how many bytes of _data are valid
  _cabort :: Bool
  -- ^ If True, the current transfer is aborted and the slave should ignore the current transfer
} deriving (Generic, ShowX, Show, NFDataX, Bundle)

toPacketStreamContent :: PacketStreamM2S dataWidth metaType -> PacketStreamContent dataWidth metaType
toPacketStreamContent PacketStreamM2S{ _data=d, _last=l, _meta=_, _abort=b } = PacketStreamContent d l b

toPacketStreamM2S :: PacketStreamContent dataWidth metaType -> metaType -> PacketStreamM2S dataWidth metaType
toPacketStreamM2S PacketStreamContent{ _cdata=d, _clast=l, _cabort=b } m = PacketStreamM2S d l m b

packetBuffer
  :: forall (dom :: Domain) (dataWidth :: Nat) (metaType :: Type) (sizeBits :: Nat) .
  HiddenClockResetEnable dom
  => KnownNat dataWidth
  => KnownNat sizeBits
  => NFDataX metaType
  => 1 <= sizeBits
  => SNat sizeBits
  -- ^ Depth of the packet buffer 2^sizeBits
  -> ( Signal dom (Maybe (PacketStreamM2S dataWidth metaType))
     , Signal dom PacketStreamS2M
     )
  -- ^ Input packetStream
  -> (  Signal dom PacketStreamS2M
     , Signal dom (Maybe (PacketStreamM2S dataWidth metaType))
     )
  -- ^ Output CSignal s
packetBuffer SNat (fwdIn, bwdIn) = (PacketStreamS2M . not <$> fullBuffer, toMaybe <$> (not <$> emptyBuffer) <*> ramOut)
  where
    ramOut = toPacketStreamM2S <$> ramContent <*> ramMeta
    --The backing ram
    ramContent = blockRam1 NoClearOnReset (SNat @(2 ^ sizeBits)) (errorX "initial block ram contents") cReadAddr' writeCommand
    ramMeta = blockRam1 NoClearOnReset (SNat @(2 ^ sizeBits)) (errorX "initial block ram contents") mReadAddr' mWriteCommand

      -- write command
    writeCommand = toMaybe <$> writeEnable <*> bundle(wordAddr, toPacketStreamContent . fromJustX <$> fwdIn)
    mWriteCommand = toMaybe <$> writeEnable <*> bundle(wordAddr, _meta . fromJustX <$> fwdIn)

    -- Registers : pointers
    wordAddr, packetAddr, readAddr :: Signal dom (Unsigned sizeBits)
    wordAddr = register 0 $ mux dropping' packetAddr $ mux writeEnable (wordAddr + 1) wordAddr
    packetAddr = register 0 $ mux (lastWordIn .&&. writeEnable) (wordAddr + 1) packetAddr
    cReadAddr' = mux readEnable (readAddr + 1) readAddr
    readAddr = register 0 cReadAddr'

    mReadAddr' = mux readEnable (readAddr + 1) readAddr

    -- Registers : status
    dropping', dropping, emptyBuffer :: Signal dom Bool
    -- start dropping packet on abort
    dropping' = abortIn .||. dropping
    dropping = register False $ dropping' .&&. (not <$> lastWordIn)
    emptyBuffer  = register 0 packetAddr .==. readAddr

    -- Only write if there is space and we're not dropping
    writeEnable = writeRequest .&&. (not <$> fullBuffer) .&&. (not <$> dropping')
    -- Read when the word has been received
    readEnable = (not <$> emptyBuffer) .&&. (_ready <$> bwdIn)

    --The status signals
    fullBuffer = (wordAddr + 1) .==. readAddr
    writeRequest = isJust <$> fwdIn
    lastWordIn = maybe False (isJust . _last) <$> fwdIn
    abortIn = maybe False _abort <$> fwdIn

abortOnBackPressure
  :: forall  (dom :: Domain) (dataWidth :: Nat) (metaType :: Type).
  HiddenClockResetEnable dom
  => KnownNat dataWidth
  => NFDataX metaType
  -- ^ Depth of the packet buffer 2^sizeBits
  => ( CSignal dom (Maybe (PacketStreamM2S dataWidth metaType))
     , Signal dom PacketStreamS2M
     )
  -- ^ Input packetStream
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
-- If a packet is larger than 2^sizeBits, the packetBuffer will have a deadlock, this should be avoided!
packetBufferC
  :: forall  (dom :: Domain)  (dataWidth :: Nat) (metaType :: Type) (sizeBits :: Nat).
  HiddenClockResetEnable dom
    => KnownNat dataWidth
    => KnownNat sizeBits
    => NFDataX metaType
    => 1 <= sizeBits
    => SNat sizeBits
    -- ^ Depth of the packet buffer, this is equal to 2^sizeBits
    -> Circuit (PacketStream dom dataWidth metaType) (PacketStream dom dataWidth metaType)
packetBufferC sizeBits = forceResetSanity |> fromSignals (packetBuffer sizeBits)

-- | A packet buffer that drops packets when it is full, instead of giving backpressure
overflowDropPacketBufferC :: forall (dataWidth :: Nat) (dom :: Domain) (metaType :: Type) (sizeBits :: Nat) .
  HiddenClockResetEnable dom
    => KnownNat dataWidth
    => NFDataX metaType
    => KnownNat sizeBits
    => 1 <= sizeBits
    => SNat sizeBits
    -- ^ Depth of the packet buffer 2^sizeBits
    -> Circuit (CSignal dom (Maybe (PacketStreamM2S dataWidth metaType))) (PacketStream dom dataWidth metaType)
overflowDropPacketBufferC size =  backPressureC |> fromSignals (packetBuffer size)
  where
    backPressureC :: Circuit (CSignal dom (Maybe (PacketStreamM2S dataWidth metaType))) (PacketStream dom dataWidth metaType)
    backPressureC = fromSignals abortOnBackPressure
