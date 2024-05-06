{-# language FlexibleContexts #-}

{- | This module contains the necessary functions and types to connect an RMII PHY to an PacketStream Protocol.

-}

module Clash.Cores.Ethernet.MII
  ( unsafeMiiRxC
  , miiTxC
  , MIIRXChannel(..)
  , MIITXChannel (..)
  )
  where

import Clash.Cores.Ethernet.PacketStream
import Clash.Prelude

import Clash.Cores.Ethernet.Util ( toMaybe )
import Data.Maybe ( isJust, isNothing )
import Protocols

-- | RX channel from the MII PHY
data MIIRXChannel domain = MIIRXChannel
  {
    mii_rx_dv :: "rxDv" ::: Signal domain Bool,
    mii_rx_err :: "rxErr" ::: Signal domain Bool,
    mii_rx_data :: "rxData" ::: Signal domain (BitVector 4)
  }

instance Protocol (MIIRXChannel domain) where
  type Fwd (MIIRXChannel domain) = MIIRXChannel domain
  type Bwd (MIIRXChannel domain) = Signal domain ()

-- | TX channel to the MII PHY
data MIITXChannel domain = MIITXChannel
  {
    mii_tx_en :: "txEn" ::: Signal domain Bool,
    mii_tx_err :: "txErr" ::: Signal domain Bool,
    mii_tx_data :: "txData" ::: Signal domain (BitVector 4)
  }

instance Protocol (MIITXChannel domain) where
  type Fwd (MIITXChannel domain) = MIITXChannel domain
  type Bwd (MIITXChannel domain) = Signal domain ()

-- | sender component of RGMII -> NOTE: for now transmission error is not considered
miiSender
  :: forall dom
   . HiddenClockResetEnable dom
  => (forall a. NFDataX a => Signal dom a -> Signal dom a)
  -- ^ tx output function
  -> ( Signal dom (Maybe (PacketStreamM2S 1 ()))
     , Signal dom ()
     )
  -> ( Signal dom PacketStreamS2M
     , MIITXChannel dom
     )
  -- ^ tx channel to the phy
miiSender fo (packet, _) = (fmap PacketStreamS2M $ inputVld .&&. nibbleSelect, channel)
  where
    inputVld = isJust <$> packet

    nibbleSelect = regEn False inputVld $ complement <$> nibbleSelect
    (ethTxDataHi, ethTxDataLo) = unbundle $ split . head . _data . fromJustX <$> packet

    channel = MIITXChannel
                { mii_tx_en = fo inputVld
                , mii_tx_err = fo $ maybe False _abort <$> packet
                , mii_tx_data = fo $ mux nibbleSelect ethTxDataLo ethTxDataHi
                }

-- | receiver component of MII
miiReceiver
  :: forall dom
   . HiddenClockResetEnable dom
  => (forall a. NFDataX a => Signal dom a -> Signal dom a)
  -- ^ Input function
  -> ( MIIRXChannel dom
     , Signal dom PacketStreamS2M
     )
  -- ^ rx channel from phy
  -> ( Signal dom ()
     , Signal dom (Maybe (PacketStreamM2S 1 ()))
     )
  -- ^ received data
miiReceiver fi (channel, _) = (pure (), packet)
  where
    dv0 = fi $ mii_rx_dv channel
    dv1 = register False dv0
    lastFragment = dv1 .&&. not <$> dv0

    err0 = fi $ mii_rx_err channel
    err1 = register undefined err0
    nibble0 = fi $ mii_rx_data channel
    nibble1 = register undefined nibble0

    -- TODO: Handle dv. This won't work yet
    -- nibbleSelect = regEn False dv0 $ complement <$> nibbleSelect

    -- Maybe the other way around?
    rxData = liftA2 (++#) nibble0 nibble1

    rawPkt = PacketStreamM2S
              <$> (singleton <$> rxData)
              <*> (toMaybe <$> lastFragment <*> 0)
              <*> pure ()
              <*> err1

    packet = toMaybe <$> dv0 <*> rawPkt

-- | Circuit that adapts a MIIRXChannel to a PacketStream.
--   This component ignores backpressure, because the MII RX PHY is unable to handle that.
unsafeMiiRxC
  :: forall dom
   . ( HiddenClockResetEnable dom
     , KnownDomain dom
     )
  => (forall a. NFDataX a => Signal dom a -> Signal dom a)
  -> Circuit (MIIRXChannel dom) (PacketStream dom 1 ())
unsafeMiiRxC fi = fromSignals (miiReceiver fi)

miiTxC
  :: forall dom
   . ( HiddenClockResetEnable dom
     , KnownDomain dom
     )
  => (forall a. NFDataX a => Signal dom a -> Signal dom a)
  -> Circuit (PacketStream dom 1 ()) (MIITXChannel dom)
miiTxC fo = fromSignals (miiSender fo)
