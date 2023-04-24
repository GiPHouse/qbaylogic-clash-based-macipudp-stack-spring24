module Clash.Cores.Ethernet.RGMII ( rgmiiSender, rgmiiReceiver, RGMIIRXChannel (..), RGMIITXChannel (..) ) where

import Clash.Prelude
import Data.Maybe ( isJust )


data RGMIIRXChannel domain ddrDomain = RGMIIRXChannel
  {
    rgmii_rx_clk :: "rx_clk" ::: Clock domain,
    rgmii_rx_ctl :: "rx_ctl" ::: Signal ddrDomain Bit,
    rgmii_rx_data :: "rx_data" ::: Signal ddrDomain (BitVector 4)
  }

data RGMIITXChannel ddrDomain = RGMIITXChannel
  {
    rgmii_tx_clk :: "tx_clk" ::: Signal ddrDomain Bit,
    rgmii_tx_ctl :: "tx_ctl" ::: Signal ddrDomain Bit,
    rgmii_tx_data :: "tx_data" ::: Signal ddrDomain (BitVector 4)
  }

-- | sender component of RGMII -> NOTE: for now transmission error is not considered
rgmiiSender :: forall dom domDDR fPeriod edge reset init polarity
  . KnownConfiguration domDDR ('DomainConfiguration domDDR fPeriod edge reset init polarity)     -- 0
  => KnownConfiguration dom ('DomainConfiguration dom (2*fPeriod) edge reset init polarity) -- 1
  => Clock dom -- the DDR tx clock
  -> Reset dom
  -> Enable dom
  -> (forall a . Signal domDDR a -> Signal domDDR a)
  -- ^ tx delay function
  -> (forall a . (NFDataX a, BitPack a) => Clock dom -> Reset dom -> Enable dom -> Signal dom a -> Signal dom a -> Signal domDDR a)
  -- ^ oddr function
  -> Signal dom (Maybe (BitVector 8))
  -- ^ Maybe the byte we have to output
  -> RGMIITXChannel domDDR
  -- ^ tx channel to the phy
rgmiiSender txClk rst en txdelay oddr input = RGMIITXChannel
  { rgmii_tx_clk = txdelay $ oddr txClk rst en (pure 1) (pure 0) -- clock forwarding
  , rgmii_tx_ctl = txdelay txCtl
  , rgmii_tx_data = txdelay txData
  }
  where
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


-- | receiver component of RGMII
rgmiiReceiver :: forall (dom :: Domain) (domDDR :: Domain) a fPeriod edge reset init polarity .
  KnownConfiguration domDDR ('DomainConfiguration domDDR fPeriod edge reset init polarity) -- 0
  => KnownConfiguration dom ('DomainConfiguration dom (2*fPeriod) edge reset init polarity) -- 1
  => RGMIIRXChannel dom domDDR
  -- ^ rx channel from phy
  -> (forall a . Signal domDDR a -> Signal domDDR a)
  -- ^ rx delay function
  -> (forall a . (NFDataX a, BitPack a) => Clock dom -> Reset dom -> Enable dom -> Signal domDDR a -> Signal dom (a, a))
  -- ^ iddr function
  -> Signal dom (Maybe (BitVector 8))
  -- ^ received data
rgmiiReceiver channel rxdelay iddr = macInput
  where
    -- extract channel
    ethRxClk :: Clock dom
    ethRxClk = rgmii_rx_clk channel
    _ethRxCtl :: Signal domDDR Bit
    _ethRxCtl = rgmii_rx_ctl channel
    _ethRxData :: Signal domDDR (BitVector 4)
    _ethRxData = rgmii_rx_data channel

    -- delay input signals
    ethRxCtl :: Signal domDDR Bit
    ethRxCtl = rxdelay _ethRxCtl
    ethRxData :: Signal domDDR (BitVector 4)
    ethRxData = rxdelay _ethRxData

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
    (ethRxData2, ethRxData1) = unbundle $ iddr ethRxClk resetGen enableGen ethRxData

    -- rgmii component -> send data to mac when rxDv is high
    macInput :: Signal dom (Maybe (BitVector 8))
    macInput = mux (bitToBool <$> ethRxDv) (fmap Just $ (++#) <$> ethRxData1 <*> ethRxData2) (pure Nothing)
