{-# language FlexibleContexts #-}

{-|
Module      : Protocols.Extra.PacketStream.DelayCircuit
Description : Circuits to delay a packet stream by a fixed amount of words, releasing all words in a packet if the "last" word is processed.
-}
module Protocols.Extra.PacketStream.DelayCircuit
  ( delayPacketStreamC
  ) where

import Clash.Prelude
import Protocols
import Protocols.DfConv ( fifo )
import Protocols.Extra.PacketStream
-- proxy
import Data.Data ( Proxy(Proxy) )
-- maybe
-- maybe
import Data.Maybe ( fromMaybe, isJust )
import Data.Maybe.Extra ( toMaybe )

data DelayState storing =
  DelayState {
    _n_items :: Index storing
    , _n_lasts :: Index storing
    , _prev_r :: Maybe Bool
    , _prev_w :: Maybe Bool
  } deriving (Generic, NFDataX)

delayPacketStreamC :: forall (dom :: Domain) (dataWidth :: Nat) (metaType :: Type) (storeI :: Nat)
  .  KnownDomain dom
  => KnownNat dataWidth
  => NFDataX metaType
  => HiddenClockResetEnable dom
  => 1 <= storeI
  => SNat storeI
  -- ^ The number of packets the buffer will delay
  -> Circuit
    (PacketStream dom dataWidth metaType)
    (PacketStream dom dataWidth metaType)
delayPacketStreamC store = forceResetSanity |> fromSignals (delayPacketStream store)

-- | A function that delays the input by a fixed amount of words, releasing al words in a packet if the "last" word is processed.
delayPacketStream
  :: forall (dom :: Domain) (dataWidth :: Nat) (metaType :: Type) (storeI :: Nat)
  .  HiddenClockResetEnable dom
  => KnownNat dataWidth
  => NFDataX metaType
  => 1 <= storeI
  => SNat storeI
  -- ^ The number of packets the buffer will delay
  -> ( Signal dom (Maybe (PacketStreamM2S dataWidth metaType))
     , Signal dom PacketStreamS2M
     )
  -> ( Signal dom PacketStreamS2M
     , Signal dom (Maybe (PacketStreamM2S dataWidth metaType))
     )
delayPacketStream storeI@SNat (fwdIn, bwdIn) = (bwdOut, fwdOut)
  where
    -- bwdOut = PacketStreamS2M <$> ((&&) <$> (_ready <$> bwdFifo) <*> (_ready <$> bwdFifoIn))
    -- fwdOut = mux r_en outFifo $ pure Nothing
    bwdFifoOut :: Signal dom PacketStreamS2M
    fwdFifoOut :: Signal dom (Maybe (PacketStreamM2S dataWidth metaType))
    (bwdFifoOut, fwdFifoOut) = toSignals fifoC (fwdFifoIn, bwdFifoIn)

    -- if read enable, connect input of the fifo to the input circuit
    fwdFifoIn :: Signal dom (Maybe (PacketStreamM2S dataWidth metaType))
    (fwdFifoIn, bwdOut) = unbundle $ mux w_en
                                      (bundle (fwdIn, bwdFifoOut))
                                      (pure (Nothing, PacketStreamS2M False))

    -- if write enable, connect output of the fifo to output circuit
    bwdFifoIn :: Signal dom PacketStreamS2M
    (bwdFifoIn, fwdOut) = unbundle $ mux r_en
                                      (bundle (bwdIn, fwdFifoOut))
                                      (pure (PacketStreamS2M False, Nothing))

    fifoC :: Circuit (PacketStream dom dataWidth metaType) (PacketStream dom dataWidth metaType)
    fifoC = fifo (Proxy :: Proxy (PacketStream dom dataWidth metaType)) (Proxy :: Proxy (PacketStream dom dataWidth metaType)) (succSNat storeI)

    w_en, r_en :: Signal dom Bool
    (w_en, r_en) = mealyB go (DelayState 0 0 Nothing Nothing) (fwdIn, bwdIn, fwdFifoOut)

    go ::
      DelayState storeI
      -> (Maybe (PacketStreamM2S dataWidth metaType), PacketStreamS2M, Maybe (PacketStreamM2S dataWidth metaType))
      -> (DelayState storeI, (Bool, Bool))
    go (DelayState storing lastAmount prev_r prev_w) (input, bwd, fifoOut) =  (nextState, (writeEnable, readEnable))
      where
        -- gaan we volgende cycle naar de buffer schrijven
        writeEnable = isJust input && storing' < maxBound
        readEnable = (lastAmount' > 0) || (storing' >= maxBound)

        nextState = DelayState storing' lastAmount' read' wrote'

        read' :: Maybe Bool
        read' = toMaybe (readEnable && _ready bwd) $ maybe False (isJust . _last) fifoOut
        -- read' = Just False
        wrote' :: Maybe Bool
        wrote' = toMaybe writeEnable $ maybe False (isJust . _last) input

        storing' =
          (if readItem then pred else id)
          . (if wroteItem then succ else id)
          $ storing

        lastAmount' =
          (if readLastItem then pred else id)
          . (if wroteLastItem then succ else id)
          $ lastAmount

        readItem = isJust prev_r
        wroteItem = isJust prev_w
        readLastItem = readItem && fromMaybe False prev_r
        wroteLastItem = wroteItem && fromMaybe False prev_w
