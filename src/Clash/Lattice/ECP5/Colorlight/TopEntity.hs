module Clash.Lattice.ECP5.Colorlight.TopEntity (topEntity) where

import Clash.Annotations.TH
import Clash.Explicit.Prelude
import Clash.Lattice.ECP5.Colorlight.CRG
import Clash.Lattice.ECP5.Prims

import Clash.Cores.UART

data TopEntityInputs = TopEntityInputs
  { clk25       :: "clk25" ::: Clock Dom25
  , uart_rx   :: "uart_rx" ::: Signal Dom50 Bit
  , sdramDqIn   :: "sdram_dq" ::: BiSignalIn 'Floating Dom50 32
  , ethMdioIn   :: "eth_mdio" ::: BiSignalIn 'Floating Dom50 1
  , eth0RxClk   :: "eth0_rx_clk" ::: Clock DomEth0
  , eth0RxCtl   :: "eth0_rx_ctl" ::: Signal DomEth0 Bit
  , eth0RxData  :: "eth0_rx_data" ::: Signal DomEth0 (BitVector 4)
  , eth1RxClk   :: "eth1_rx_clk" ::: Clock DomEth1
  , eth1RxCtl   :: "eth1_rx_ctl" ::: Signal DomEth1 Bit
  , eth1RxData  :: "eth1_rx_data" ::: Signal DomEth1 (BitVector 4)
  }

data TopEntityOutputs = TopEntityOutputs
  { uart_tx           :: "uart_tx" ::: Signal Dom50 Bit
  , sdram_clock       :: "sdram_clock" ::: Clock Dom50
  , sdram_a           :: "sdram_a" ::: Signal Dom50 (BitVector 11)
  , sdram_we_n        :: "sdram_we_n" ::: Signal Dom50 Bit
  , sdram_ras_n       :: "sdram_ras_n" ::: Signal Dom50 Bit
  , sdram_cas_n       :: "sdram_cas_n" ::: Signal Dom50 Bit
  , sdram_ba          :: "sdram_ba" ::: Signal Dom50 (BitVector 2)
  , sdram_dq          :: "sdram_dq" ::: BiSignalOut 'Floating Dom50 32
  , eth_mdio          :: "eth_mdio" ::: BiSignalOut 'Floating Dom50 1
  , eth_mdc           :: "eth_mdc" ::: Signal Dom50 Bit
  , eth_rst_n         :: "eth_rst_n" ::: Signal Dom50 Bit
  , eth0_tx_clk       :: "eth0_tx_clk" ::: Clock DomEth0
  , eth0_tx_ctl       :: "eth0_tx_ctl" ::: Signal DomEth0 Bit
  , eth0_tx_data      :: "eth0_tx_data" ::: Signal DomEth0 (BitVector 4)
  , eth1_tx_clk       :: "eth1_tx_clk" ::: Clock DomEth1
  , eth1_tx_ctl       :: "eth1_tx_ctl" ::: Signal DomEth1 Bit
  , eth1_tx_data      :: "eth1_tx_data" ::: Signal DomEth1 (BitVector 4)
  , hub_clk           :: "hub_clk" ::: Signal Dom50 Bit
  , hub_line_select   :: "hub_line_select" ::: Signal Dom50 (BitVector 5)
  , hub_latch         :: "hub_latch" ::: Signal Dom50 Bit
  , hub_output_enable :: "hub_output_enable" ::: Signal Dom50 Bit
  , hub_data          :: "hub_data" ::: Signal Dom50 (BitVector 48)
  }

topEntity :: TopEntityInputs -> TopEntityOutputs
topEntity inputs =
  let
    TopEntityInputs { clk25       = _clk25
                    , uart_rx     = uartRxBit
                    , sdramDqIn   = dq_in
                    , ethMdioIn   = mdio_in
                    , eth0RxClk   = _eth0RxClk
                    , eth0RxCtl   = _eth0RxCtl
                    , eth0RxData  = _eth0RxData
                    , eth1RxClk   = _eth1RxClk
                    , eth1RxCtl   = _eth1RxCtl
                    , eth1RxData  = _eth1RxData
                    } = inputs

    (clk50, rst50) = crg _clk25
    en50 = enableGen

    -- Simply echo back uart signals through IO flip flops
    uartTxBit = ofs1p3bx clk50 rst50 en50 $ ifs1p3bx clk50 rst50 en50 uartRxBit

    -- Bidirectional signals require special care.
    -- As an example below we switch between reading from the signal
    -- in one cycle and writing to it in the next.
    -- We combine this with IO flip flops.
    -- We simply write back the signal we got.
    dq :: Signal Dom50 (BitVector 32)
    mdio :: Signal Dom50 Bit
    (dq_out, dq) = bb dq_in onoff (ofs1p3bx clk50 rst50 en50 dqReg) -- sdram_dq
    (mdio_out, mdio) = bb mdio_in onoff (ofs1p3bx clk50 rst50 en50 mdioReg) -- sdram_dq

    dqReg = ifs1p3bx clk50 rst50 en50 dq
    mdioReg = ifs1p3bx clk50 rst50 en50 mdio

    onoff = register clk50 rst50 en50 0 $ fmap complement onoff
  in
    TopEntityOutputs { uart_tx             = uartTxBit
                     , sdram_clock        = clk50
                     , sdram_a            = pure 0
                     , sdram_we_n         = pure 1
                     , sdram_ras_n        = pure 1
                     , sdram_cas_n        = pure 1
                     , sdram_ba           = pure 0
                     , sdram_dq           = dq_out
                     , eth_mdio           = mdio_out
                     , eth_mdc            = pure 0
                     , eth_rst_n          = pure 1
                     , eth0_tx_clk        = _eth0RxClk
                     , eth0_tx_ctl        = pure 0
                     , eth0_tx_data       = pure 0
                     , eth1_tx_clk        = _eth1RxClk
                     , eth1_tx_ctl        = pure 0
                     , eth1_tx_data       = pure 0
                     , hub_clk            = pure 0
                     , hub_line_select    = pure 0
                     , hub_latch          = pure 0
                     , hub_output_enable  = pure 0
                     , hub_data           = pure 0
                     }

makeTopEntity 'topEntity
