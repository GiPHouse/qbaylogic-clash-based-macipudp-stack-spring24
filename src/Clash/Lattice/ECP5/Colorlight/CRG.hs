{-# language BangPatterns #-}
{-# language QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Clash.Lattice.ECP5.Colorlight.CRG where

import Clash.Annotations.Primitive
import Clash.Explicit.Prelude
import Data.String.Interpolate ( i )
import Data.String.Interpolate.Util ( unindent )

createDomain vSystem
  { vName="Dom25"
  , vPeriod=40000
  , vActiveEdge=Rising
  , vResetKind=Synchronous
  , vInitBehavior=Unknown
  , vResetPolarity=ActiveHigh
  }

createDomain vSystem
  { vName="Dom50"
  , vPeriod=20000
  , vActiveEdge=Rising
  , vResetKind=Synchronous
  , vInitBehavior=Unknown
  , vResetPolarity=ActiveHigh
  }

createDomain vSystem
  { vName="DomEth0"
  , vPeriod=8000
  , vActiveEdge=Rising
  , vResetKind=Synchronous
  , vInitBehavior=Unknown
  , vResetPolarity=ActiveHigh
  }

createDomain vSystem
  { vName="DomDDREth0"
  , vPeriod=4000
  , vActiveEdge=Rising
  , vResetKind=Synchronous
  , vInitBehavior=Unknown
  , vResetPolarity=ActiveHigh
  }

createDomain vSystem
  { vName="DomEth1"
  , vPeriod=8000
  , vActiveEdge=Rising
  , vResetKind=Synchronous
  , vInitBehavior=Unknown
  , vResetPolarity=ActiveHigh
  }

createDomain vSystem
  { vName="DomDDREth1"
  , vPeriod=4000
  , vActiveEdge=Rising
  , vResetKind=Synchronous
  , vInitBehavior=Unknown
  , vResetPolarity=ActiveHigh
  }

createDomain vSystem
  { vName="DomEthTx"
  , vPeriod=8000
  , vActiveEdge=Rising
  , vResetKind=Synchronous
  , vInitBehavior=Unknown
  , vResetPolarity=ActiveHigh
  }

-- | Simple Clock reset generator for the colorlight ECP5 board
--   Generated using ecppll utility
{-# NOINLINE crg #-}
crg
  :: Clock Dom25
  -- ^ Input clock
  -> (Clock Dom50, Clock DomEthTx, Reset Dom50, Reset DomEthTx)
     -- ^ Output clock and reset
crg clkin = (clk50, clkEthTx, rst50, rstEthTx)
  where
    (clk50, locked50) = pll50 clkin
    (clkEthTx, lockedEthTx) = pll125 clkin
    rst50 = resetSynchronizer clk50 (unsafeFromLowPolarity locked50)
    rstEthTx = resetSynchronizer clkEthTx (unsafeFromLowPolarity lockedEthTx)

-- | Generate a 50Mhz clock from 25Mhz
pll50
  :: Clock Dom25
  -- ^ Input 25 Mhz clock
  -> (Clock Dom50, Signal Dom50 Bool)
  -- ^ Output 50Mhz clock and unsynchronized reset signal
pll50 !_ = (clockGen, unsafeToLowPolarity resetGen)
{-# ANN pll50 (InlinePrimitive [Verilog] $ unindent [i|
  [ { "BlackBox" :
      { "name"     : "Clash.Lattice.ECP5.Colorlight.CRG.pll50"
      , "kind"     : "Declaration"
      , "template" :
  "// pll50 primary begin
  wire ~GENSYM[clk50][0];
  wire ~GENSYM[pll50_locked][1];

  (* FREQUENCY_PIN_CLKI=\\"25\\" *)
  (* FREQUENCY_PIN_CLKOP=\\"50\\" *)
  (* ICP_CURRENT=\\"12\\" *) (* LPF_RESISTOR=\\"8\\" *) (* MFG_ENABLE_FILTEROPAMP=\\"1\\" *) (* MFG_GMCREF_SEL=\\"2\\" *)
  EHXPLLL #(
    .PLLRST_ENA(\\"DISABLED\\"),
    .INTFB_WAKE(\\"DISABLED\\"),
    .STDBY_ENABLE(\\"DISABLED\\"),
    .DPHASE_SOURCE(\\"DISABLED\\"),
    .OUTDIVIDER_MUXA(\\"DIVA\\"),
    .OUTDIVIDER_MUXB(\\"DIVB\\"),
    .OUTDIVIDER_MUXC(\\"DIVC\\"),
    .OUTDIVIDER_MUXD(\\"DIVD\\"),
    .CLKI_DIV(1),
    .CLKOP_ENABLE(\\"ENABLED\\"),
    .CLKOP_DIV(12),
    .CLKOP_CPHASE(5),
    .CLKOP_FPHASE(0),
    .FEEDBK_PATH(\\"CLKOP\\"),
    .CLKFB_DIV(2)
  ) ~GENSYM[pll50_inst][2] (
    .RST(1'b0),
    .STDBY(1'b0),
    .CLKI(~ARG[0]),
    .CLKOP(~SYM[0]),
    .CLKFB(~SYM[0]),
    .CLKINTFB(),
    .PHASESEL0(1'b0),
    .PHASESEL1(1'b0),
    .PHASEDIR(1'b1),
    .PHASESTEP(1'b1),
    .PHASELOADREG(1'b1),
    .PLLWAKESYNC(1'b0),
    .ENCLKOP(1'b0),
    .LOCK(~SYM[1])
	);

  assign ~RESULT = {~SYM[0], ~SYM[1]};
  // pll50 primary end"
      }
    }
  ]
  |]) #-}
{-# NOINLINE pll50 #-}

-- | Generate a 125Mhz clock from 25Mhz
pll125
  :: Clock Dom25
  -- ^ Input 25 Mhz clock
  -> (Clock DomEthTx, Signal DomEthTx Bool)
  -- ^ Output 125Mhz clock and unsynchronized reset signal
pll125 !_ = (clockGen, unsafeToLowPolarity resetGen)
{-# ANN pll125 (InlinePrimitive [Verilog] $ unindent [i|
  [ { "BlackBox" :
      { "name"     : "Clash.Lattice.ECP5.Colorlight.CRG.pll125"
      , "kind"     : "Declaration"
      , "template" :
  "// pll125 primary begin
  wire ~GENSYM[clkEthTx][0];
  wire ~GENSYM[pll125_locked][1];

  (* FREQUENCY_PIN_CLKI=\\"25\\" *)
  (* FREQUENCY_PIN_CLKOP=\\"125\\" *)
  (* ICP_CURRENT=\\"12\\" *) (* LPF_RESISTOR=\\"8\\" *) (* MFG_ENABLE_FILTEROPAMP=\\"1\\" *) (* MFG_GMCREF_SEL=\\"2\\" *)
  EHXPLLL #(
          .PLLRST_ENA(\\"DISABLED\\"),
          .INTFB_WAKE(\\"DISABLED\\"),
          .STDBY_ENABLE(\\"DISABLED\\"),
          .DPHASE_SOURCE(\\"DISABLED\\"),
          .OUTDIVIDER_MUXA(\\"DIVA\\"),
          .OUTDIVIDER_MUXB(\\"DIVB\\"),
          .OUTDIVIDER_MUXC(\\"DIVC\\"),
          .OUTDIVIDER_MUXD(\\"DIVD\\"),
          .CLKI_DIV(1),
          .CLKOP_ENABLE(\\"ENABLED\\"),
          .CLKOP_DIV(5),
          .CLKOP_CPHASE(2),
          .CLKOP_FPHASE(0),
          .FEEDBK_PATH(\\"CLKOP\\"),
          .CLKFB_DIV(5)
      ) ~GENSYM[pll125_inst][2] (
          .RST(1'b0),
          .STDBY(1'b0),
          .CLKI(~ARG[0]),
          .CLKOP(~SYM[0]),
          .CLKFB(~SYM[0]),
          .CLKINTFB(),
          .PHASESEL0(1'b0),
          .PHASESEL1(1'b0),
          .PHASEDIR(1'b1),
          .PHASESTEP(1'b1),
          .PHASELOADREG(1'b1),
          .PLLWAKESYNC(1'b0),
          .ENCLKOP(1'b0),
          .LOCK(~SYM[1])
    );

  assign ~RESULT = {~SYM[0], ~SYM[1]};
  // pll125 primary end"
      }
    }
  ]
  |]) #-}
{-# NOINLINE pll125 #-}
