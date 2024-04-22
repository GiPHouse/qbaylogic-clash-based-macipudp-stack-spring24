{-# language FlexibleContexts #-}

module Clash.Cores.Ethernet.PacketBuffer
    ( packetBuffer
    , packetBufferC
    ) where

import Clash.Cores.Ethernet.PacketStream
import Clash.Cores.Ethernet.Util
import Clash.Prelude
import Data.Maybe

import Protocols ( Circuit(..), fromSignals, (|>) )
import Protocols.Internal ( CSignal(..) )

packetBuffer
  :: forall (dataWidth :: Nat) (sizeBits :: Nat) (dom :: Domain) (metaType :: Type).
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
  -> Signal dom (Maybe (PacketStreamM2S dataWidth metaType))
  -- ^ Output CSignal s
packetBuffer SNat (fwdIn, bwdIn) = toMaybe <$> notEmpty <*> ramOut
  where
    --The backing ram
    ramOut = blockRam1 NoClearOnReset (SNat @(2 ^ sizeBits)) (errorX "initial block ram contents") readAddr' writeCommand

      -- write command
    writeCommand = func <$> writeEnable <*> fwdIn <*> wordAddr
      where
        func False _          _     = Nothing
        func _     Nothing    _     = Nothing
        func _     (Just dat) wordAddr = Just (wordAddr, dat)

    -- Registers : pointers
    wordAddr, packetAddr, readAddr :: Signal dom (Unsigned sizeBits)
    wordAddr = register 0 $ mux dropping packetAddr $ mux writeEnable (wordAddr + 1) wordAddr
    packetAddr = register 0 $ mux (lastWord .&&. writeEnable) (wordAddr + 1) packetAddr
    readAddr' = mux readEnable (readAddr + 1) readAddr
    readAddr = register 0 readAddr'

    -- Registers : status
    dropping, emptyBuffer :: Signal dom (Bool)
    dropping = register False $ (fullBuffer .&&. writeRequest) .||. (dropping .&&. (not <$> lastWord))
    emptyBuffer  = (register 0 packetAddr) .==. readAddr

    -- Only write if there is space
    writeEnable = writeRequest .&&. (not <$> fullBuffer) .&&. (not <$> dropping)
    -- Read when the word has been received
    readEnable = notEmpty .&&. (_ready <$> bwdIn)
    notEmpty = not <$> emptyBuffer

    --The status signals
    fullBuffer = (wordAddr + 1)  .==. readAddr
    writeRequest = isJust <$> fwdIn
    lastWord = isLast <$> fwdIn

    isLast :: Maybe (PacketStreamM2S dataWidth metaType) -> Bool
    isLast word = case word of
        Just (PacketStreamM2S { _last = Just _ }) -> True
        _ -> False

-- | PacketBuffer Circuit, does not generate backpressure on the RHS, but instead drops packets
-- Respects backpressure on the LHS
packetBufferC
  :: forall (dataWidth :: Nat) (sizeBits :: Nat) (dom :: Domain) (metaType :: Type).
  HiddenClockResetEnable dom
    => KnownNat dataWidth
    => KnownNat sizeBits
    => NFDataX metaType
    => 1 <= sizeBits
    -- ^ Depth of the packet buffer 2^sizeBits
    => SNat sizeBits
    -- ^ Packet buffer circuit
    -> Circuit (PacketStream dom dataWidth metaType) (PacketStream dom dataWidth metaType)
packetBufferC sizeBits = forceResetSanity |> fromPacketStream |> fromSignals wrap
  where
    wrap:: ( CSignal dom (Maybe (PacketStreamM2S dataWidth metaType)),
                Signal dom PacketStreamS2M
            )
        -> (CSignal dom (), Signal dom (Maybe (PacketStreamM2S dataWidth metaType)))
    wrap (CSignal fwdIn, bwdIn)  = (CSignal (pure ()), packetBuffer sizeBits (fwdIn, bwdIn))
