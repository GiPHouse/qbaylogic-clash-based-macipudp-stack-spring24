{-# language FlexibleContexts #-}

{- | This module contains the necessary functions and types to connect an RGMII PHY to an Axi4Stream interface.

To keep this module generic users will have to provide their own "primitive" functions:
1. `delay` functions set to the proper amount of delay (which can be different for RX and TX)
2. `iddr` function to turn a single DDR (Double Data Rate) signal into 2 non-DDR signals
2. `oddr` function to turn two non-DDR signals into a single DDR signal

Note that clash models a DDR signal as being twice as fast, thus both facilitating and requiring type-level separation between the two "clock domains".
-}

module Clash.Cores.Ethernet.RGMII ( unsafeRgmiiCircuits, rgmiiSender, unsafeRgmiiReceiver, RGMIIRXChannel (..), RGMIITXChannel (..) ) where

import Clash.Prelude
import Data.Maybe ( isJust )

import Clash.Cores.Ethernet.Stream ( SingleByteStream )
import Protocols
import Protocols.Axi4.Stream
import Protocols.Internal ( CSignal(..) )

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

instance Protocol (RGMIIRXChannel dom domDDR) where
    type Fwd (RGMIIRXChannel dom domDDR) = RGMIIRXChannel dom domDDR
    type Bwd (RGMIIRXChannel dom domDDR) = ()

instance Protocol (RGMIITXChannel dom) where
    type Fwd (RGMIITXChannel dom) = RGMIITXChannel dom
    type Bwd (RGMIITXChannel dom) = ()

-- | Convenience function for getting the circuits of both `rgmiiSender` and `unsafeRgmiiReceiver`.
--
-- Only the `unsafeRgmiiReceiver` does not handle backpressure.
-- `rgmiiSender` does not have backpressure and is thus safe to use.
unsafeRgmiiCircuits :: forall dom domDDR . KnownDomain domDDR
  => KnownDomain dom
  => Clock dom
  -> Reset dom
  -> Enable dom
  -> (forall a . Signal domDDR a -> Signal domDDR a)
  -- ^ rx delay needed
  -> (forall a . Signal domDDR a -> Signal domDDR a)
  -- ^ tx delay function
  -> (forall a . (NFDataX a, BitPack a) => Clock dom -> Reset dom -> Enable dom -> Signal domDDR a -> Signal dom (a, a))
  -- ^ iddr function
  -> (forall a . (NFDataX a, BitPack a) => Clock dom -> Reset dom -> Enable dom -> Signal dom a -> Signal dom a -> Signal domDDR a)
  -- ^ oddr function
  -> ( Circuit (RGMIIRXChannel dom domDDR) (SingleByteStream dom)
     , Circuit (SingleByteStream dom) (RGMIITXChannel domDDR)
     )
unsafeRgmiiCircuits clk rst en rxDelay txDelay iddr oddr = (unsafeRgmiiReceiver clk rst en rxDelay iddr, rgmiiSender clk rst en txDelay oddr)

-- | Sender component from Axi4Stream to RGMII
--
-- `_tready` is guaranteed to be set to `True`.
--
-- NOTE: for now transmission error is not considered
rgmiiSender :: forall dom domDDR . KnownDomain domDDR
  => KnownDomain dom
  => Clock dom
  -> Reset dom
  -> Enable dom
  -> (forall a . Signal domDDR a -> Signal domDDR a)
  -- ^ tx delay function
  -> (forall a . (NFDataX a, BitPack a) => Clock dom -> Reset dom -> Enable dom -> Signal dom a -> Signal dom a -> Signal domDDR a)
  -- ^ oddr function
  -> Circuit (SingleByteStream dom) (RGMIITXChannel domDDR)
  -- ^ tx channel to the phy
rgmiiSender txClk rst en txdelay oddr = Circuit circuitFunction where
    circuitFunction (axiInput,_) = (pure Axi4StreamS2M {_tready = True}, RGMIITXChannel
      { rgmii_tx_clk = txdelay $ oddr txClk rst en (pure 1) (pure 0) -- clock forwarding
      , rgmii_tx_ctl = txdelay txCtl
      , rgmii_tx_data = txdelay txData
      }) where
        input :: Signal dom (Maybe (Unsigned 8))
        input = fmap (head . _tdata) <$> axiInput
        txEn, txErr :: Signal dom Bit
        txEn = boolToBit . isJust <$> input -- set tx_en high
        txErr = pure 0 -- for now error always low
        data1, data2 :: Signal dom (BitVector 4)
        (data1, data2) = unbundle $ maybe (0, 0) split <$> input

        -- multiplex signals
        txCtl :: Signal domDDR Bit
        txCtl = oddr txClk rst en txEn txCtlFalling
            where
                -- The TXCTL signal at the falling edge is the XOR of TXEN and TXERR
                -- meaning that TXERR is the XOR of it and TXEN.
                -- See RGMII interface documentation.
                txCtlFalling = xor <$> txEn <*> txErr
        txData :: Signal domDDR (BitVector 4)
        -- LSB first! See RGMII interface documentation.
        txData = oddr txClk rst en data2 data1


-- | Receiver component from RGMII to Axi4Stream
--
-- Sets `_tlast` to `True` iff this is the last byte in an Ethernet frame, see `unsafeToAxi`.
--
-- Does not handle backpressure!
unsafeRgmiiReceiver :: forall dom domDDR . KnownDomain domDDR
  => KnownDomain dom
  => Clock dom
  -> Reset dom
  -> Enable dom
  -> (forall a . Signal domDDR a -> Signal domDDR a)
  -- ^ rx delay function
  -> (forall a . (NFDataX a, BitPack a) => Clock dom -> Reset dom -> Enable dom -> Signal domDDR a -> Signal dom (a, a))
  -- ^ iddr function
  -> Circuit (RGMIIRXChannel dom domDDR) (SingleByteStream dom)
unsafeRgmiiReceiver clk rst en rxdelay iddr = (withClockResetEnable clk rst en unsafeToAxi) <| unsafeRgmiiReceiverRaw rxdelay iddr

-- Transforms RGMII rxDv, rxErr and rxData signals into an `Axi4Stream`
--
-- Processed according to the RGMII specification:
-- See table 4 of https://web.pa.msu.edu/hep/atlas/l1calo/hub/hardware/components/micrel/rgmii_specification_hp_v1.3_dec_2000.pdf
--
-- Does not handle backpressure!
unsafeToAxi :: HiddenClockResetEnable dom => Circuit (CSignal dom (Bool, Bool, Unsigned 8)) (SingleByteStream dom)
unsafeToAxi = Circuit circuitFunction
  where
    circuitFunction (CSignal inp, _recvACK) = (CSignal $ pure (), mapOutput <$> prev <*> inp)
      where
        prev = register (False, False, 0) inp
    mapOutput (rxDv, rxErr, rxData) (nextRxDv, nextRxErr, _) = out
      where
        out = case (rxDv, rxErr) of
                (True, False) -> Just Axi4StreamM2S { _tdata = singleton rxData
                                                    , _tkeep = singleton True
                                                    , _tstrb = singleton False
                                                    , _tlast = not (nextRxDv && not nextRxErr)
                                                    , _tuser = ()
                                                    , _tid = 0
                                                    , _tdest = 0
                                                    }
                _ -> Nothing

-- | Extracts rxDv, rxErr and rxData from an `RGMIIRXChannel` and returns it as a `CSignal`
--
-- Does not handle backpressure!
unsafeRgmiiReceiverRaw :: forall dom domDDR . KnownDomain domDDR
  => KnownDomain dom
  => (forall a . Signal domDDR a -> Signal domDDR a)
  -- ^ rx delay function
  -> (forall a . (NFDataX a, BitPack a) => Clock dom -> Reset dom -> Enable dom -> Signal domDDR a -> Signal dom (a, a))
  -- ^ iddr function
  -> Circuit (RGMIIRXChannel dom domDDR) (CSignal dom (Bool, Bool, Unsigned 8))
unsafeRgmiiReceiverRaw rxdelay iddr = Circuit circuitFunction
  where
    circuitFunction (channel, _) = ((), CSignal $ bundle (bitToBool <$> ethRxDv, bitToBool <$> ethRxErr, ethRxData))
     where
      -- extract channel
      ethRxClk :: Clock dom
      ethRxClk = rgmii_rx_clk channel
      ethRxCtl :: Signal domDDR Bit
      ethRxCtl = rxdelay $ rgmii_rx_ctl channel
      ethRxDataDDR :: Signal domDDR (BitVector 4)
      ethRxDataDDR = rxdelay $ rgmii_rx_data channel

      -- demultiplex signal
      ethRxDv, ethRxErr :: Signal dom Bit
      (ethRxDv, ethRxErr) = unbundle $ fmap handleCtl $ iddr ethRxClk resetGen enableGen ethRxCtl
          where
              -- The RXCTL signal at the falling edge is the XOR of RXDV and RXERR
              -- meaning that RXERR is the XOR of it and RXDV.
              -- See RGMII interface documentation.
              handleCtl :: (Bit, Bit) -> (Bit, Bit)
              handleCtl (dv,err) = (dv, dv `xor` err)
      ethRxData1, ethRxData2 :: Signal dom (BitVector 4)
      -- LSB first! See RGMII interface documentation.
      (ethRxData2, ethRxData1) = unbundle $ iddr ethRxClk resetGen enableGen ethRxDataDDR

      -- Merge data into 1 whole byte
      ethRxData :: Signal dom (Unsigned 8)
      ethRxData = fmap unpack $ (++#) <$> ethRxData1 <*> ethRxData2
