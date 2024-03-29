{-# language FlexibleContexts #-}

{- | This module contains the necessary functions and types to connect an RGMII PHY to an Axi4Stream interface.

To keep this module generic users will have to provide their own "primitive" functions:
1. `delay` functions set to the proper amount of delay (which can be different for RX and TX)
2. `iddr` function to turn a single DDR (Double Data Rate) signal into 2 non-DDR signals
2. `oddr` function to turn two non-DDR signals into a single DDR signal

Note that clash models a DDR signal as being twice as fast, thus both facilitating
and requiring type-level separation between the two "clock domains".
-}

module Clash.Cores.Ethernet.RGMII
  ( rgmiiSender
  , rgmiiReceiver
  , RGMIIRXChannel(..)
  , RGMIITXChannel (..)
  ) where

import Clash.Prelude
import Data.Maybe 

import Clash.Cores.Ethernet.PacketStream
import Protocols


-- | RX channel from the RGMII PHY
data RGMIIRXChannel domain ddrDomain = RGMIIRXChannel
  {
    rgmii_rx_clk :: "rx_clk" ::: Clock domain,
    rgmii_rx_ctl :: "rx_ctl" ::: Signal ddrDomain Bit,
    rgmii_rx_data :: "rx_data" ::: Signal ddrDomain (BitVector 4)
  }

-- | TX channel to the RGMII PHY
data RGMIITXChannel ddrDomain = RGMIITXChannel
  {
    rgmii_tx_clk :: "tx_clk" ::: Signal ddrDomain Bit,
    rgmii_tx_ctl :: "tx_ctl" ::: Signal ddrDomain Bit,
    rgmii_tx_data :: "tx_data" ::: Signal ddrDomain (BitVector 4)
  }
-- | sender component of RGMII -> NOTE: for now transmission error is not considered
rgmiiSender
  :: forall dom domDDR fPeriod edge reset init polarity
   . KnownConfiguration domDDR ('DomainConfiguration domDDR fPeriod edge reset init polarity)
  => KnownConfiguration dom ('DomainConfiguration dom (2*fPeriod) edge reset init polarity)
  => Clock dom
  -- ^ the DDR tx clock
  -> Reset dom
  -> (forall a. Signal domDDR a -> Signal domDDR a)
  -- ^ tx delay function
  -> (forall a. (NFDataX a, BitPack a) => Clock dom -> Reset dom -> Signal dom a -> Signal dom a -> Signal domDDR a)
  -- ^ oddr function
  -> Signal dom (Maybe (BitVector 8))
  -- ^ Maybe the byte we have to output
  -> RGMIITXChannel domDDR
  -- ^ tx channel to the phy
rgmiiSender txClk rst txdelay oddr input = channel
  where
    txEn, txErr :: Signal dom Bit
    txEn = boolToBit . isJust <$> input -- set tx_en high
    txErr = pure 0 -- for now error always low
    ethTxData1, ethTxData2 :: Signal dom (BitVector 4)
    (ethTxData1, ethTxData2) = unbundle $ maybe (undefined, undefined) split <$> input

    -- multiplex signals
    -- The TXCTL signal at the falling edge is the XOR of TXEN and TXERR
    -- meaning that TXERR is the XOR of it and TXEN.
    -- See RGMII interface documentation.
    txCtl :: Signal domDDR Bit
    txCtl = oddr txClk rst txEn (xor <$> txEn <*> txErr)
    txData :: Signal domDDR (BitVector 4)
    -- LSB first! See RGMII interface documentation.
    txData = oddr txClk rst ethTxData2 ethTxData1

    channel = RGMIITXChannel
                { rgmii_tx_clk = txdelay $ oddr txClk rst (pure 1) (pure 0)
                , rgmii_tx_ctl = txdelay txCtl
                , rgmii_tx_data = txdelay txData
                }


-- TODO: Maybe make utility module or use existing package
toMaybe :: Bool -> a -> Maybe a
toMaybe True x = Just x
toMaybe False _ = Nothing

-- | receiver component of RGMII
rgmiiReceiver
  :: forall dom domDDR
   . (2 * DomainPeriod domDDR ~ DomainPeriod dom, KnownDomain dom)
  => RGMIIRXChannel dom domDDR
  -- ^ rx channel from phy
  -> (forall a. Signal domDDR a -> Signal domDDR a)
  -- ^ rx delay function
  -> (forall a. (NFDataX a, BitPack a) => Clock dom -> Reset dom -> Signal domDDR a -> Signal dom (a, a))
  -- ^ iddr function
  -> Signal dom (Bool, Maybe (BitVector 8))
  -- ^ received data
rgmiiReceiver channel rxdelay iddr = bundle (ethRxErr, byteStream)
  where
    ethRxClk :: Clock dom
    ethRxClk = rgmii_rx_clk channel

    -- input path
    ethRxCtl :: Signal dom (Bool, Bool)
    ethRxCtl =  iddr ethRxClk resetGen $ rxdelay $ fmap bitToBool $ rgmii_rx_ctl channel
    ethRxData :: Signal dom (BitVector 4, BitVector 4)
    ethRxData = iddr ethRxClk resetGen $ rxdelay $ rgmii_rx_data channel

    -- demultiplex rxctl
    -- The RXCTL signal at the falling edge is the XOR of RXDV and RXERR
    -- meaning that RXERR is the XOR of it and RXDV.
    -- See RGMII interface documentation.
    ethRxDv, ethRxErr :: Signal dom Bool
    (ethRxDv, ethRxErr) = unbundle $ fmap (\(dv, err) -> (dv, dv `xor` err)) ethRxCtl

    -- LSB first! See RGMII interface documentation.
    ethRxData1, ethRxData2 :: Signal dom (BitVector 4)
    (ethRxData2, ethRxData1) = unbundle ethRxData
    rxData = liftA2 (++#) ethRxData1 ethRxData2

    byteStream :: Signal dom (Maybe (BitVector 8))
    byteStream = toMaybe <$> ethRxDv <*> rxData

newtype RgmiiRxAdapterState = RgmiiRxAdapterState { _last_byte :: Maybe (BitVector 8) }
  deriving (Generic, NFDataX)

instance Protocol (RGMIIRXChannel domain ddrDomain) where
  type Fwd (RGMIIRXChannel domain ddrDomain) = RGMIIRXChannel domain ddrDomain
  type Bwd (RGMIIRXChannel domain ddrDomain) = Signal domain ()

-- | Circuit that adapts a RGMIIRXChannel into a PacketStream
unsafeRgmiiRxC :: forall dom domDDR
   . HiddenClockResetEnable dom
  => KnownDomain dom
  => 2 * DomainPeriod domDDR ~ DomainPeriod dom
  => RGMIIRXChannel dom domDDR
  -- ^ rx channel from phy
  -> (forall a. Signal domDDR a -> Signal domDDR a)
  -- ^ rx delay function
  -> (forall a. (NFDataX a, BitPack a) => Clock dom -> Reset dom -> Signal domDDR a -> Signal dom (a, a))
  -- ^ iddr function
  -> Circuit (RGMIIRXChannel dom domDDR) (PacketStream dom 1 ())
unsafeRgmiiRxC channel rxdelay iddr = fromSignals ckt
    where
        inputS = rgmiiReceiver channel rxdelay iddr
        ckt (_, bwdIn) = mealyB stateFunc s0 (inputS, bwdIn)
          where
            s0 = RgmiiRxAdapterState { _last_byte = Nothing }
            stateFunc s ((rxErr, inp), PacketStreamS2M bwd) = (nextState, ((), out))
              where
                nextState = RgmiiRxAdapterState { _last_byte = inp }
                -- drop packets when receiving backpressure
                out = if not bwd then Nothing else fmap go (_last_byte s)
                go x = PacketStreamM2S {
                    _data = x :> Nil,
                    _last = if isNothing inp then Just 0 else Nothing,
                    _meta = (),
                    _abort = rxErr
                }
