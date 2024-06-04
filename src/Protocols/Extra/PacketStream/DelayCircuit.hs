{-# language FlexibleContexts #-}

{-|
Module      : Protocols.Extra.PacketStream.DelayCircuit
Description : Circuits to delay a packet stream by a fixed amount of words, releasing all words in a packet if the "last" word is processed.
-}
module Protocols.Extra.PacketStream.DelayCircuit
  ( bufferIC
  ) where

import Clash.Prelude
import Protocols
import Protocols.DfConv ( fifo )
import Protocols.Extra.PacketStream
-- proxy
import Data.Data ( Proxy(Proxy) )
-- maybe
import Data.Maybe ( isJust )

bufferIC :: forall (dom :: Domain) (dataWidth :: Nat) (storeI :: Nat)
  .  KnownDomain dom
  => KnownNat dataWidth
  => HiddenClockResetEnable dom
  => 1 <= storeI
  => SNat storeI
  -- ^ The number of packets the buffer will delay
  -> Circuit
    (PacketStream dom dataWidth ())
    (PacketStream dom dataWidth ())
bufferIC store = forceResetSanity |> fromSignals (bufferI store)

data BufferIState storing =
  BufferIState {
    _storing :: Index storing
    , _lastAmount :: Index storing
    , _read :: Bool
    , _wasLastr :: Bool
    , _wrote :: Bool
    , _wasReadw :: Bool 
  } deriving (Generic, NFDataX)
-- | A buffer that delays the input by a fixed amount of words, releasing al words in a packet if the "last" word is processed.
bufferI
  :: forall (dom :: Domain) (dataWidth :: Nat) (storeI :: Nat)
  .  HiddenClockResetEnable dom
  => KnownNat dataWidth
  => 1 <= storeI
  => SNat storeI
  -- ^ The number of packets the buffer will delay
  -> ( Signal dom (Maybe (PacketStreamM2S dataWidth ()))
     , Signal dom PacketStreamS2M
     )
  -> ( Signal dom PacketStreamS2M
     , Signal dom (Maybe (PacketStreamM2S dataWidth ()))
     )
bufferI storeI@SNat (fwdIn, bwdIn) = (bwdOut, fwdOut)
  where
    bwdOut = PacketStreamS2M <$> ((&&) <$> (_ready <$> bwdFifo) <*> (_ready <$> bwdFifoIn))
    fwdOut = mux r_en outFifo $ pure Nothing

    bwdFifo :: Signal dom PacketStreamS2M
    outFifo :: Signal dom (Maybe (PacketStreamM2S dataWidth ()))
    (bwdFifo, outFifo) = toSignals fifoC (fwdFifoIn, bwdFifoIn)

    fifoC :: Circuit (PacketStream dom dataWidth ()) (PacketStream dom dataWidth ())
    fifoC = fifo (Proxy :: Proxy (PacketStream dom dataWidth ())) (Proxy :: Proxy (PacketStream dom dataWidth ())) (succSNat storeI)

    fwdFifoIn :: Signal dom (Maybe (PacketStreamM2S dataWidth ()))
    fwdFifoIn = mux w_en fwdIn $ pure Nothing
    bwdFifoIn :: Signal dom PacketStreamS2M
    bwdFifoIn = mux readPrevious bwdIn $ pure (PacketStreamS2M False)
    readPrevious = register False r_en

    w_en, r_en :: Signal dom Bool
    (w_en, r_en) = unbundle $ mealy go (BufferIState 0 0 False False False False) (bundle (fwdIn, bwdIn, outFifo))

    go ::
      BufferIState storeI
      -> (Maybe (PacketStreamM2S dataWidth ()), PacketStreamS2M, Maybe (PacketStreamM2S dataWidth ()))
      -> (BufferIState storeI, (Bool, Bool))
    go (BufferIState storing lastAmount wasRead wasLastr wrote wasLastw) (input, bwd, fifoOut) =  (nextSt, out)
      where
        out = (writeEnable, readEnable)

        nextSt = BufferIState storing' lastAmount' readEnable wasLastr' writeEnable wasLastw'

        wasLastr' = readEnable && maybe False (isJust . _last) fifoOut
        wasLastw' = writeEnable && maybe False (isJust . _last) input

        -- gaan we volgende cycle naar de buffer schrijven
        writeEnable = isJust input && storing' < maxBound
        -- storing < maxBound, write the word to buffer 
        -- we are not allowed to assume that the word we output will be acked in one go, 
        --    so maximum throughput is not allowed
        readEnable = (lastAmount' > 0) || (storing' >= maxBound) 
        -- we mogen lezen 
        --    als het einde van een pakket in de buffer zit
        --    of als we het benodigde aantal words hebben 
        --    of als we een minder dan maxBound words hebben en we gaan ook schrijven

        -- storing is vorige state
        -- storing' is huidige state
        storing' =
          (if readItem then pred else id)
          . (if wrote then succ else id)
          $ storing
        -- lastAmount = vorige state
        -- lastAmount' = current state
        lastAmount' =
          (if readItem && wasLastr then pred else id)
          . (if wrote && wasLastw then succ else id)
          $ lastAmount

        readItem = wasRead && _ready bwd 
