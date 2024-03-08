{-# language FlexibleContexts #-}

module Clash.Cores.Ethernet.PacketBuffer
    ( packetBuffer
    ) where

import Clash.Cores.Ethernet.PacketStream
import Clash.Prelude
import Data.Maybe

packetBuffer
    :: forall (dataWidth :: Nat) (sizeBits :: Nat) (dom :: Domain) (metaType :: Type).
    HiddenClockResetEnable dom
    => KnownNat dataWidth
    => KnownNat sizeBits
    => NFDataX metaType
    => 1 <= sizeBits
    => SNat sizeBits
    -> ( Signal dom (Maybe (PacketStreamM2S dataWidth metaType)),
            Signal dom PacketStreamS2M
        )
    -> (Signal dom (), Signal dom (Maybe (PacketStreamM2S dataWidth metaType)))

packetBuffer SNat (inM2S, inS2M) =  (pure (), outM2S)
    where
        --The backing ram
        outM2S = blockRam1 NoClearOnReset (SNat @(2 ^ sizeBits)) (errorX "initial block ram contents") readAddr' writeCommand

         -- write command
        writeCommand = mux writeEnable (Just <$> bundle (wordAddr, inM2S)) (pure Nothing)

        --The read and write pointers
        wordAddr, packetAddr, readAddr :: Signal dom (Unsigned sizeBits)
        wordAddr = register 0 $ mux writeEnable (wordAddr + 1) wordAddr
        packetAddr = register 0 $ mux lastWord wordAddr packetAddr
        readAddr' = mux readEnable (readAddr + 1) readAddr
        readAddr = register 0 readAddr'

        -- Only write if there is space
        writeEnable = writeRequest .&&. fmap not full .&&. fmap not dropping
        readEnable = (_ready <$> inS2M) .&&. fmap not emptyBuffer

        --The status signals
        emptyBuffer  = register 0 packetAddr .==. readAddr
        full   = readAddr .==. (packetAddr + 1)
        dropping = register False ((full .&&. writeRequest) .||. (dropping .&&. fmap not lastWord))
        writeRequest = isJust <$> inM2S
        lastWord = isLast <$> inM2S
        isLast :: Maybe (PacketStreamM2S dataWidth metaType) -> Bool
        isLast word = case word of
            Just (PacketStreamM2S { _last = Just _ }) -> True
            _ -> False
