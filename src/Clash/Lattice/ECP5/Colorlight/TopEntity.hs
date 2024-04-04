{-# language NumericUnderscores #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Clash.Lattice.ECP5.Colorlight.TopEntity ( topEntity ) where

import Clash.Annotations.TH
import Clash.Cores.Ethernet.RGMII
    ( RGMIIRXChannel(..), RGMIITXChannel(..), rgmiiReceiver, rgmiiSender )
import Clash.Explicit.Prelude
import Clash.Lattice.ECP5.Colorlight.CRG
import Clash.Lattice.ECP5.Prims
import Clash.Lattice.ECP5.Colorlight.UartEthRxStack
import Clash.Cores.UART (baudGenerator)
import Clash.Prelude (exposeClockResetEnable)
data SDRAMOut domain = SDRAMOut
  {
    sdram_clock :: "clk" :::Clock domain,
    sdram_a :: "a" ::: Signal domain (BitVector 11),
    sdram_we_n :: "we_n" ::: Signal domain Bit,
    sdram_ras_n :: "ras_n" :::Signal domain Bit,
    sdram_cas_n :: "cas_n" ::: Signal domain Bit,
    sdram_ba :: "ba" ::: Signal domain (BitVector 2),
    sdram_dq :: "dq" ::: BiSignalOut 'Floating domain 32
  }

data MDIOOut domain = MDIOOut
  {
    mdio_out :: "mdio" ::: BiSignalOut 'Floating domain 1,
    mdio_mdc :: "mdc" ::: Signal domain Bit
  }

data HubOut domain = HubOut
  {
    hub_clk :: "clk" ::: Signal domain Bit,
    hub_line_select :: "line_select" ::: Signal domain (BitVector 5),
    hub_latch :: "latch" ::: Signal domain Bit,
    hub_output_enable :: "output_enable" ::: Signal domain Bit,
    hub_data :: "data" ::: Signal domain (BitVector 48)
  }

topEntity
  :: "clk25" ::: Clock Dom25
  -> "uart_rx" ::: Signal Dom50 Bit
  -> "sdram_dq" ::: BiSignalIn 'Floating Dom50 32
  -> "eth_mdio" ::: BiSignalIn 'Floating Dom50 1
  -> "eth0" ::: RGMIIRXChannel DomEth0 DomDDREth0
  -> "eth1" ::: RGMIIRXChannel DomEth1 DomDDREth1
  -> ( "uart_tx" ::: Signal Dom50 Bit
     , "sdram" ::: SDRAMOut Dom50
     , "eth" ::: MDIOOut Dom50
     , "eth0" ::: RGMIITXChannel DomDDREth0
     , "eth1" ::: RGMIITXChannel DomDDREth1
     , "hub" ::: HubOut Dom50
     )
topEntity clk25 _uartRxBit _dq_in _mdio_in eth0_rx eth1_rx =
  let
    (clk50, _clkEthTx, rst50, _rstEthTx) = crg clk25
    en50 = enableGen

    baudGen = exposeClockResetEnable (baudGenerator (SNat @115200)) clk50 rst50 en50
    uartTxBit = exposeClockResetEnable (uartEthRxStack baudGen eth0_rx) clk50 rst50 en50

    {- ETH0 ~ RGMII transceivers -}
    eth0Txclk = rgmii_rx_clk eth0_rx
    eth0Tx = rgmiiSender eth0Txclk resetGen (delayg d0) oddrx1f (pure Nothing)

    {- ETH1 ~ RGMII transceivers -}
    eth1Txclk = rgmii_rx_clk eth1_rx
    (_eth1Err, eth1Data) = unbundle $ rgmiiReceiver eth1_rx (delayg d80) iddrx1f
    eth1Tx = rgmiiSender eth1Txclk resetGen (delayg d0) oddrx1f eth1Data

    in
      ( uartTxBit
      , SDRAMOut
          { sdram_clock = clk50
          , sdram_a = pure 0
          , sdram_we_n = pure 1
          , sdram_ras_n = pure 1
          , sdram_cas_n = pure 1
          , sdram_ba = pure 0
          , sdram_dq = undefined
          }
      , MDIOOut
          { mdio_out = undefined
          , mdio_mdc = pure 0
          }
      , eth0Tx
      , eth1Tx
      , HubOut
          { hub_clk = pure 0
          , hub_line_select = pure 0
          , hub_latch = pure 0
          , hub_output_enable = pure 0
          , hub_data = pure 0
          }
      )

makeTopEntity 'topEntity
