{-# language FlexibleContexts #-}

{- |
Module      : Clash.Lattice.ECP5.RGMII
Description : Functions and types to connect an RGMII PHY to a packet stream interface.

To keep this module generic users will have to provide their own "primitive" functions:

    1. delay functions set to the proper amount of delay (which can be different for RX and TX);
    2. iddr function to turn a single DDR (Double Data Rate) signal into 2 non-DDR signals;
    3. oddr function to turn two non-DDR signals into a single DDR signal.

Note that Clash models a DDR signal as being twice as fast, thus both facilitating
and requiring type-level separation between the two "clock domains".
-}

module Clash.Lattice.ECP5.RGMII
  ( rgmiiSender
  , rgmiiReceiver
  , unsafeRgmiiRxC
  , RGMIIRXChannel(..)
  , RGMIITXChannel (..)
  , rgmiiTxC
  ) where

import Clash.Prelude
import Protocols.Extra.PacketStream

import Data.Maybe ( isJust, isNothing )
import Data.Maybe.Extra ( toMaybe )
import Protocols

-- | RX channel from the RGMII PHY
data RGMIIRXChannel domain ddrDomain = RGMIIRXChannel
  {
    rgmii_rx_clk :: "rx_clk" ::: Clock domain,
    rgmii_rx_ctl :: "rx_ctl" ::: Signal ddrDomain Bit,
    rgmii_rx_data :: "rx_data" ::: Signal ddrDomain (BitVector 4)
  }

instance Protocol (RGMIIRXChannel domain ddrDomain) where
  type Fwd (RGMIIRXChannel domain ddrDomain) = RGMIIRXChannel domain ddrDomain
  type Bwd (RGMIIRXChannel domain ddrDomain) = Signal domain ()

-- | TX channel to the RGMII PHY
data RGMIITXChannel ddrDomain = RGMIITXChannel
  {
    rgmii_tx_clk :: "tx_clk" ::: Signal ddrDomain Bit,
    rgmii_tx_ctl :: "tx_ctl" ::: Signal ddrDomain Bit,
    rgmii_tx_data :: "tx_data" ::: Signal ddrDomain (BitVector 4)
  }

instance Protocol (RGMIITXChannel ddrDomain) where
  type Fwd (RGMIITXChannel ddrDomain) = RGMIITXChannel ddrDomain
  type Bwd (RGMIITXChannel ddrDomain) = Signal ddrDomain ()

-- | RGMII sender. Does not consider transmission error.
rgmiiSender
  :: forall dom domDDR
   . 2 * DomainPeriod domDDR ~ DomainPeriod dom
  => Clock dom
  -- ^ the DDR tx clock
  -> Reset dom
  -> (forall a. Signal domDDR a -> Signal domDDR a)
  -- ^ tx delay function
  -> (forall a. (NFDataX a, BitPack a) => Clock dom -> Reset dom -> Signal dom a -> Signal dom a -> Signal domDDR a)
  -- ^ oddr function
  -> Signal dom (Maybe (BitVector 8))
  -- ^ Maybe the byte we have to output
  -> Signal dom Bool
  -- ^ Error signal indicating whether the current packet is corrupt
  -> RGMIITXChannel domDDR
  -- ^ Tx channel to the PHY
rgmiiSender txClk rst txdelay oddr input err = channel
  where
    txEn, txErr :: Signal dom Bit
    txEn = boolToBit . isJust <$> input -- set tx_en high
    txErr = fmap boolToBit err

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

-- | RGMII receiver.
rgmiiReceiver
  :: forall dom domDDR
   . (2 * DomainPeriod domDDR ~ DomainPeriod dom, KnownDomain dom)
  => RGMIIRXChannel dom domDDR
  -- ^ rx channel from PHY
  -> (forall a. Signal domDDR a -> Signal domDDR a)
  -- ^ rx delay function
  -> (forall a. (NFDataX a, BitPack a) => Clock dom -> Reset dom -> Signal domDDR a -> Signal dom (a, a))
  -- ^ iddr function
  -> Signal dom (Bool, Maybe (BitVector 8))
  -- ^ Received data and error bit
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

data RgmiiRxAdapterState = RgmiiRxAdapterState {
  _last_byte :: Maybe (BitVector 8),
  _last_rx_err :: Bool
} deriving (Show, Generic, NFDataX)

-- | Circuit that adapts a RGMIIRXChannel to a PacketStream.
-- This component ignores backpressure, because the RGMII RX PHY is unable to handle that.
unsafeRgmiiRxC :: forall dom domDDR .
  ( HiddenClockResetEnable dom
  , KnownDomain dom
  , 2 * DomainPeriod domDDR ~ DomainPeriod dom)
  => (forall a. Signal domDDR a -> Signal domDDR a)
  -- ^ rx delay function
  -> (forall a. (NFDataX a, BitPack a) => Clock dom -> Reset dom -> Signal domDDR a -> Signal dom (a, a))
  -- ^ iddr function
  -> Circuit (RGMIIRXChannel dom domDDR) (PacketStream dom 1 ())
unsafeRgmiiRxC rxdelay iddr = fromSignals ckt
  where
    ckt (fwdIn, bwdIn) = mealyB stateFunc s0 (rgmiiReceiver fwdIn rxdelay iddr, bwdIn)
    s0 = RgmiiRxAdapterState {_last_byte = Nothing, _last_rx_err = False}
    stateFunc s ((rxErr, byte), _) = (nextState, ((), fwdOut))
      where
        nextState = RgmiiRxAdapterState {_last_byte = byte, _last_rx_err = rxErr}
        {- Forward the RGMII receiver's inputs from last clock cycle.
           If the last byte from the RGMII receiver was valid and the current one is not valid, we set _last.
           If the RGMII receiver gives an error, we assert abort. -}
        fwdOut = fmap go (_last_byte s)
        go x = PacketStreamM2S {
            _data = x :> Nil,
            _last = toMaybe (isNothing byte) 0,
            _meta = (),
            _abort = _last_rx_err s
        }

-- | RGMII transmit circuit
rgmiiTxC
  :: forall dom domDDR
   . HiddenClockResetEnable dom
  => KnownDomain dom
  => 2 * DomainPeriod domDDR ~ DomainPeriod dom
  => (forall a. Signal domDDR a -> Signal domDDR a)
  -- ^ tx delay function
  -> (forall a. (NFDataX a, BitPack a) => Clock dom -> Reset dom -> Signal dom a -> Signal dom a -> Signal domDDR a)
  -- ^ oddr function
  -> Circuit (PacketStream dom 1 ()) (RGMIITXChannel domDDR)
  -- ^ circuit that transforms a PacketStream to a RGMIITXChannel
rgmiiTxC txDelay oddr = fromSignals go
  where
    go (fwdIn, _) = (bwdOut, fwdOut)
      where
        getTx = fmap $ fmap (head . _data)
        getErr = fmap (maybe False _abort)
        input = getTx fwdIn
        err = getErr fwdIn
        bwdOut = pure $ PacketStreamS2M {_ready = True}
        fwdOut = rgmiiSender hasClock hasReset txDelay oddr input err

