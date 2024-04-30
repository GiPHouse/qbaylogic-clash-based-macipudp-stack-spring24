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
  :: forall (dom :: Domain) (dataWidth :: Nat) (metaType :: Type) (contentSizeBits :: Nat) (metaSizeBits :: Nat) .
  HiddenClockResetEnable dom
  => KnownNat dataWidth
  => KnownNat contentSizeBits => KnownNat metaSizeBits
  => 1 <= contentSizeBits => 1 <= metaSizeBits
  => NFDataX metaType
  -- ^ Depth for the content of the packet buffer 2^contentSizeBits
  => SNat contentSizeBits
  -> SNat metaSizeBits
  -- ^ Depth for the meta of the packet buffer 
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
    -- The backing ram
    fwdOut = toMaybe <$> (not <$> emptyBuffer) <*> (toPacketStreamM2S <$> ramContent <*> ramMeta)
    ramContent = blockRam1 NoClearOnReset (SNat @(2 ^ contentSizeBits)) (errorX "initial block ram content") cReadAddr' writeCommand
    ramMeta = blockRam1 NoClearOnReset (SNat @(2 ^ metaSizeBits)) (errorX "initial block ram meta content") mReadAddr' mWriteCommand

      -- write command
    writeCommand = toMaybe <$> writeEnable <*> bundle(cWordAddr, toPacketStreamContent . fromJustX <$> fwdIn)
    mWriteCommand = toMaybe <$> mWriteEnable <*> bundle(mWriteAddr, _meta . fromJustX <$> fwdIn)

    -- Registers : pointers
    -- Addresses for the content
    cWordAddr, cPacketAddr, cReadAddr :: Signal dom (Unsigned contentSizeBits)
    cWordAddr = register 0 $ mux dropping' cPacketAddr $ mux writeEnable (cWordAddr + 1) cWordAddr
    cPacketAddr = register 0 $ mux (lastWordIn .&&. writeEnable) (cWordAddr + 1) cPacketAddr
    cReadAddr' = mux readEnable (cReadAddr + 1) cReadAddr
    cReadAddr = register 0 cReadAddr'

    -- Addresses for the meta content
    mWriteAddr, mReadAddr :: Signal dom (Unsigned metaSizeBits)
    mWriteAddr = register 0 mWriteAddr'
    mWriteAddr' = mux mWriteEnable (mWriteAddr + 1) mWriteAddr

    mReadAddr' = mux mReadEnable (mReadAddr + 1) mReadAddr
    mReadAddr = register 0 mReadAddr'

    mWriteEnable = lastWordIn .&&. writeEnable
    mReadEnable = lastWordOut .&&. readEnable

    -- Registers : status
    dropping', dropping, emptyBuffer :: Signal dom Bool
    -- start dropping packet on abort
    dropping' = abortIn .||. dropping
    dropping = register False $ dropping' .&&. (not <$> lastWordIn)
    emptyBuffer = (register 0 cPacketAddr .==. cReadAddr) .||. (register 0 mWriteAddr .==. mReadAddr)

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
  :: forall  (dom :: Domain)  (dataWidth :: Nat) (metaType :: Type) 
      (contentSizeBits :: Nat) (metaSizeBits :: Nat).
  HiddenClockResetEnable dom
    => KnownNat dataWidth
    => KnownNat contentSizeBits => KnownNat metaSizeBits
    => 1 <= contentSizeBits => 1 <= metaSizeBits
    => NFDataX metaType
    => SNat contentSizeBits
    -> SNat metaSizeBits
    -- ^ Depth of the packet buffer, this is equal to 2^sizeBits
    -> Circuit (PacketStream dom dataWidth metaType) (PacketStream dom dataWidth metaType)
packetBufferC cSizeBits mSizeBits = forceResetSanity |> fromSignals (packetBuffer cSizeBits mSizeBits)

-- | A packet buffer that drops packets when it is full, instead of giving backpressure
overflowDropPacketBufferC 
  :: forall  (dom :: Domain)  (dataWidth :: Nat) (metaType :: Type) 
      (contentSizeBits :: Nat) (metaSizeBits :: Nat).
  HiddenClockResetEnable dom
    => KnownNat dataWidth
    => KnownNat contentSizeBits => KnownNat metaSizeBits
    => 1 <= contentSizeBits => 1 <= metaSizeBits
    => NFDataX metaType
    => SNat contentSizeBits
    -> SNat metaSizeBits
    -- ^ Depth of the packet buffer 2^sizeBits
    -> Circuit (CSignal dom (Maybe (PacketStreamM2S dataWidth metaType))) (PacketStream dom dataWidth metaType)
overflowDropPacketBufferC cSizeBits mSizeBits =  backPressureC |> fromSignals (packetBuffer cSizeBits mSizeBits)
  where
    backPressureC :: Circuit (CSignal dom (Maybe (PacketStreamM2S dataWidth metaType))) (PacketStream dom dataWidth metaType)
    backPressureC = fromSignals abortOnBackPressure
