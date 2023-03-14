module Clash.Lattice.ECP5.Colorlight.TopEntity (topEntity) where

import Clash.Annotations.TH
import Clash.Explicit.Prelude
import Clash.Lattice.ECP5.Colorlight.CRG
import Clash.Lattice.ECP5.Prims

import Clash.Cores.UART

data RGMIIChannel domain = RGMIIChannel
  {
    rgmii_rx_clk :: Clock domain,
    rgmii_rx_ctl :: Signal domain Bit,
    rgmii_rx_data :: Signal domain (BitVector 4),
    rgmii_tx_clk :: Clock domain,
    rgmii_tx_ctl :: Signal domain Bit,
    rgmii_tx_data :: Signal domain (BitVector 4)
  }

data SDRAMOut domain = SDRAMOut
  {
    sdram_clock :: "sdram_clock" :::Clock domain,
    sdram_a :: "sdram_a" ::: Signal domain (BitVector 11),
    sdram_we_n :: "sdram_we_n" ::: Signal domain Bit,
    sdram_ras_n :: "sdram_ras_n" :::Signal domain Bit,
    sdram_cas_n :: "sdram_cas_n" ::: Signal domain Bit,
    sdram_ba :: "sdram_ba" ::: Signal domain (BitVector 2),
    sdram_dq :: "sdram_dq" ::: BiSignalOut 'Floating domain 32
  }

data MDIOOut domain = MDIOOut
  {
    mdio_out :: "eth_mdio" ::: BiSignalOut 'Floating domain 1,
    mdio_in :: "eth_mdio" ::: BiSignalIn 'Floating domain 1,
    mdio_mdc :: "eth_mdc" ::: Signal domain Bit
  }

data HubOut domain = HubOut
  {
    hub_clk :: "hub_clk" ::: Signal domain Bit,
    hub_line_select :: "hub_line_select" ::: Signal domain (BitVector 5),
    hub_latch :: "hub_latch" ::: Signal domain Bit,
    hub_output_enable :: "hub_output_enable" ::: Signal domain Bit,
    hub_data :: "hub_data" ::: Signal domain (BitVector 48)
  }

topEntity
  :: "clk25" ::: Clock Dom25
  -> "uart_rx" ::: Signal Dom50 Bit
  -> "sdram_dq" ::: BiSignalIn 'Floating Dom50 32
  -> "eth_mdio" ::: BiSignalIn 'Floating Dom50 1
  -> "eth0_rx" ::: RGMIIChannel DomEth0
  -> "eth1_rx" ::: RGMIIChannel DomEth1
  -> ( "uart_tx" ::: Signal Dom50 Bit
     , "sdram" ::: SDRAMOut Dom50
     , "eth" ::: MDIOOut Dom50
     , "eth0_tx" ::: RGMIIChannel DomEth0
     , "eth1_tx" ::: RGMIIChannel DomEth1
     , "hub" ::: HubOut Dom50
     )

topEntity clk25 uartRxBit dq_in _mdio_in eth0_rx eth1_rx =
  let
    RGMIIChannel { rgmii_rx_clk = eth0RxClk
                 , rgmii_rx_ctl = _eth0RxCtl
                 , rgmii_rx_data = _eth0RxData
                 } = eth0_rx

    RGMIIChannel { rgmii_rx_clk = eth1RxClk
                , rgmii_rx_ctl = _eth1RxCtl
                , rgmii_rx_data = _eth1RxData
                } = eth1_rx

    (clk50, rst50) = crg clk25
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
    (eth_mdio_out, mdio) = bb _mdio_in onoff (ofs1p3bx clk50 rst50 en50 mdioReg) -- sdram_dq

    dqReg = ifs1p3bx clk50 rst50 en50 dq
    mdioReg = ifs1p3bx clk50 rst50 en50 mdio

    onoff = register clk50 rst50 en50 0 $ fmap complement onoff

    in
      ( uartTxBit
      , SDRAMOut
          { sdram_clock = clk50
          , sdram_a = pure 0
          , sdram_we_n = pure 1
          , sdram_ras_n = pure 1
          , sdram_cas_n = pure 1
          , sdram_ba = pure 0
          , sdram_dq = dq_out
          }
      , MDIOOut
          { mdio_out = eth_mdio_out
          , mdio_in = _mdio_in
          , mdio_mdc = pure 0
          }
      , RGMIIChannel  -- eth0
          { rgmii_tx_clk = eth0RxClk
          , rgmii_tx_ctl = pure 0
          , rgmii_tx_data = pure 0
          , rgmii_rx_clk = eth0RxClk
          , rgmii_rx_ctl = _eth0RxCtl
          , rgmii_rx_data = _eth0RxData
          }
      , RGMIIChannel  --eth1
          { rgmii_tx_clk = eth1RxClk
          , rgmii_tx_ctl = pure 0
          , rgmii_tx_data = pure 0
          , rgmii_rx_clk = eth1RxClk
          , rgmii_rx_ctl = _eth1RxCtl
          , rgmii_rx_data = _eth1RxData
          }
      , HubOut
          { hub_clk = pure 0
          , hub_line_select = pure 0
          , hub_latch = pure 0
          , hub_output_enable = pure 0
          , hub_data = pure 0
          }
      )

makeTopEntity 'topEntity
