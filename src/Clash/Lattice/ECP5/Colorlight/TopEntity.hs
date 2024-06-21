{-# language FlexibleContexts #-}
{-# language MultiParamTypeClasses #-}
{-# language NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Module      : Clash.Lattice.ECP5.Colorlight.TopEntity
Description : Contains the top entity.
-}
module Clash.Lattice.ECP5.Colorlight.TopEntity
  ( topEntity
  ) where

import Clash.Annotations.TH

import Clash.Explicit.Prelude
import Clash.Prelude ( exposeClockResetEnable )

import Clash.Cores.Crc ( deriveHardwareCrc )
import Clash.Cores.Crc.Catalog ( Crc32_ethernet )
import Clash.Cores.Ethernet.IP.IPv4Types ( IPv4Address(IPv4Address) )
import Clash.Cores.Ethernet.Mac.EthernetTypes ( MacAddress(MacAddress) )
import Clash.Lattice.ECP5.Colorlight.CRG
import Clash.Lattice.ECP5.Prims
import Clash.Lattice.ECP5.RGMII ( RGMIIRXChannel(..), RGMIITXChannel(..), rgmiiTxC, unsafeRgmiiRxC )

import Protocols ( toSignals, (|>) )

import Clash.Cores.Ethernet.Examples.FullUdpStack ( fullStackC )
import Data.Proxy ( Proxy(Proxy) )


$(deriveHardwareCrc (Proxy @Crc32_ethernet) d8 d4)

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

-- | The top entity
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
topEntity clk25 uartRxBit _dq_in _mdio_in eth0_rx _eth1_rx =
  let
    (clk50, ethTxClk, rst50, ethTxRst) = crg clk25
    en50 = enableGen

    ethRxClk = rgmii_rx_clk eth0_rx
    ethRxRst = resetGen @DomEth0
    ethRxEn = enableGen @DomEth0
    ethTxEn = enableGen @DomEthTx

    -- Replace this with your FPGA's MAC address
    ourMac = MacAddress (0x00 :> 0x00 :> 0x00 :> 0xff :> 0xff :> 0xff :> Nil)
    -- Hardcoded IPv4 and subnet mash
    ourIPv4 = ( IPv4Address (192 :> 168 :> 1 :> 123 :> Nil)
              , IPv4Address (255 :> 255 :> 255 :> 0 :> Nil)
              )

    phyStack
      = exposeClockResetEnable (unsafeRgmiiRxC @DomEth0 @DomDDREth0 (delayg d80) iddrx1f) ethRxClk ethRxRst ethRxEn
        |> exposeClockResetEnable (fullStackC ethRxClk ethRxRst ethRxEn ethTxClk ethTxRst ethTxEn (pure ourMac) (pure ourIPv4)) clk50 rst50 en50
        |> exposeClockResetEnable (rgmiiTxC @DomEthTx @DomDDREth0 (delayg d0) oddrx1f) ethTxClk ethTxRst ethTxEn

    uartTxBit = uartRxBit

    eth0Tx = snd $ toSignals phyStack (eth0_rx, pure ())

    {- ETH1 ~ RGMII transceivers -}
    eth1Tx = undefined

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
