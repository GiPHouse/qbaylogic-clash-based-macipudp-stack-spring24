{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

{-|
Module      : Clash.Lattice.ECP5.Prims
Description : Primitives for the Lattice ECP5

All of the primitives in this module are taken from the Lattice ECP5
Library reference: https://www.latticesemi.com/-/media/LatticeSemi/Documents/UserManuals/EI2/fpga_library_D311SP3.ashx?document_id=52656
-}
module Clash.Lattice.ECP5.Prims
  ( bb
  , ofs1p3bx
  , ifs1p3bx
  , oddrx1f
  , iddrx1f
  , delayf
  , delayg
  ) where

import Clash.Annotations.Primitive
import Clash.Explicit.DDR (ddrIn, ddrOut)
import Clash.Explicit.Prelude
import Clash.Signal.BiSignal
import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)
import GHC.Stack (HasCallStack)

import Data.Kind (Type)

-- | Bidirectional buffer primitive
bb
  :: forall
    (ds :: BiSignalDefault)
    (dom :: Domain)
    (a :: Type)
   . HasCallStack
  => KnownDomain dom
  => HasBiSignalDefault ds
  => BitPack a
  => NFDataX a
  => BiSignalIn ds dom (BitSize a)
  -- ^ BiSignal to write to
  -> Signal dom Bit
  -- ^ Output enable, active low
  -> Signal dom a
  -- ^ Data to write if output enable is low
  -> ( BiSignalOut ds dom (BitSize a) -- output BiSignal
     , Signal dom a -- input
     )
bb pkgPinOut nOE output = bb# primName pkgPinOut nOE output
 where
  primName = case pullUpMode pkgPinOut of
    SFloating -> "BB"
    SPullUp -> "BBPU"
    SPullDown -> "BBPD"

bb#
  :: forall
    (ds :: BiSignalDefault)
    (dom :: Domain)
    (a :: Type)
   . HasCallStack -- 0
  => KnownDomain dom -- 1
  => BitPack a -- 2
  => NFDataX a -- 3
  => String -- 4
  -> BiSignalIn ds dom (BitSize a) -- 5
  -> Signal dom Bit -- 6
  -> Signal dom a -- 7
  -> ( BiSignalOut ds dom (BitSize a)
     , Signal dom a
     )
bb# !_ pkgPinIn nOE output = (pkgPinOut, dIn)
 where
  dIn = readFromBiSignal pkgPinIn
  toMaybe True a = Just a
  toMaybe False _ = Nothing
  pkgPinOut = writeToBiSignal pkgPinIn (toMaybe . not . bitToBool <$> nOE <*> output)
{-# NOINLINE bb# #-}
{-# ANN
  bb#
  ( InlinePrimitive [Verilog]
      $ unindent
        [i|
  [ { "BlackBox" :
      { "name"     : "Clash.Lattice.ECP5.Prims.bb#"
      , "kind"     : "Declaration"
      , "template" :
  "// ~NAME[4] begin
  ~IF~ISSCALAR[7]~THEN
    ~NAME[4] ~GENSYM[~NAME[4]_INST][3] (
        .I(~VAR[inD][7]),
        .T(~VAR[nOE][6]),
        .O(~RESULT),
        .B(~ARG[5])
      );
  ~ELSE
    genvar ~GENSYM[i][0];
    for (~SYM[0]=0; ~SYM[0] < ~SIZE[~TYP[7]]; ~SYM[0]=~SYM[0]+1) begin : ~GENSYM[~NAME[4]_GEN][1]
      ~NAME[4] ~GENSYM[~NAME[4]_INST][2] (
        .I(~VAR[inD][7][~SYM[0]]),
        .T(~VAR[nOE][6]),
        .O(~RESULT[~SYM[0]]),
        .B(~ARG[5][~SYM[0]])
      );
    end
  ~FI
  // ~NAME[4] end"
      }
    }
  ]
  |]
  )
  #-}

-- | PIC output flip flop with Asynchronous clear
ofs1p3bx
  :: KnownDomain dom
  => BitPack a
  => NFDataX a
  => Clock dom
  -- ^ Clock signal
  -> Reset dom
  -- ^ Reset signal
  -> Enable dom
  -- ^ Enable signal
  -> Signal dom a
  -- ^ Data input from fabric
  -> Signal dom a
  -- ^ Output to pin
ofs1p3bx clk rst en inp = fs1p3bx# "O" clk (unsafeToHighPolarity rst) (fromEnable en) inp

-- | PIC input flip flip with Asynchronous clear
ifs1p3bx
  :: KnownDomain dom
  => BitPack a
  => NFDataX a
  => Clock dom
  -> Reset dom
  -> Enable dom
  -> Signal dom a
  -- ^ Data input from pin
  -> Signal dom a
  -- ^ Output to fabric
ifs1p3bx clk rst en inp = fs1p3bx# "I" clk (unsafeToHighPolarity rst) (fromEnable en) inp

fs1p3bx#
  :: KnownDomain dom -- 0
  => BitPack a -- 1
  => NFDataX a -- 2
  => String -- 3

  -- ^ I or O depending on input or output
  -> Clock dom -- 4

  -- ^ Clock
  -> Signal dom Bool -- 5

  -- ^ Reset, active high
  -> Signal dom Bool -- 6

  -- ^ enable, active high
  -> Signal dom a -- 7

  -- ^ Data input from pin or output register block
  -> Signal dom a
  -- ^ Output
fs1p3bx# !_ clk rst en inp =
  let rst' = unsafeFromHighPolarity rst
      en' = toEnable en
      -- Reset value is defined as 1
      resetVal = unpack $ complement 0
   in register clk rst' en' resetVal inp
{-# ANN
  fs1p3bx#
  ( InlinePrimitive [Verilog]
      $ unindent
        [i|
  [ { "BlackBox" :
      { "name"     : "Clash.Lattice.ECP5.Prims.fs1p3bx#"
      , "kind"     : "Declaration"
      , "template" :
  "// ~NAME[3]FS1P3BX# begin
  ~IF~ISSCALAR[7]~THEN
    ~NAME[3]FS1P3BX ~GENSYM[~NAME[3]FS1P3BX_INST][3] (
      .D(~VAR[d][7]),  // Data
      .SP(~ARG[6]),    // Enable
      .SCLK(~ARG[4]),  // Clk
      .PD(~ARG[5]),    // Preset
      .Q(~RESULT)      // Output
    );
  ~ELSE
    genvar ~GENSYM[i][0];
    for (~SYM[0] = 0; ~SYM[0] < ~SIZE[~TYP[7]]; ~SYM[0] = ~SYM[0] + 1) begin : ~GENSYM[~NAME[3]FS1P3BX_GEN][1]
      ~NAME[3]FS1P3BX ~GENSYM[~NAME[3]FS1P3BX_INST][2] (
        .D(~VAR[d][7][~SYM[0]]),  // Data
        .SP(~ARG[6]),          // Enable
        .SCLK(~ARG[4]),        // Clk
        .PD(~ARG[6]),          // Preset
        .Q(~RESULT[~SYM[0]])   // Output
      );
  end
  ~FI
  // ~NAME[3]FS1P3BX# end"
      }
    }
  ]
  |]
  )
  #-}
{-# NOINLINE fs1p3bx# #-}

-- | x1 input DDR
iddrx1f
  :: KnownConfiguration fast ('DomainConfiguration fast fPeriod edge reset init polarity) -- 0
  => KnownConfiguration slow ('DomainConfiguration slow (2 * fPeriod) edge reset init polarity) -- 1
  => NFDataX a -- 2
  => BitPack a -- 3
  => Clock slow -- 4

  -- ^ Clock
  -> Reset slow -- 5

  -- ^ Reset
  -> Signal fast a -- 6

  -- ^ Input
  -> Signal slow (a, a)
  -- ^ Output on rising/falling edge
iddrx1f clk rst xs = ddrIn clk rst enableGen (unpack 0, unpack 0, unpack 0) xs
{-# ANN
  iddrx1f
  ( InlinePrimitive [Verilog]
      $ unindent
        [i|
  [ { "BlackBox" :
      { "name"     : "Clash.Lattice.ECP5.Prims.iddrx1f"
      , "kind"     : "Declaration"
      , "template" :
  "// IDDRX1F begin
  ~IF~ISSCALAR[6]~THEN
    IDDRX1F ~GENSYM[iddrx1f][0] (
      .D(~VAR[d][6]),
      .SCLK(~ARG[4]),
      .RST(~ARG[5]),
      .Q0(~RESULT[0]),
      .Q1(~RESULT[1])
    );
  ~ELSE
    genvar ~GENSYM[i][1];
    wire [~SIZE[~TYP[6]]-1:0] ~GENSYM[q0][4];
    wire [~SIZE[~TYP[6]]-1:0] ~GENSYM[q1][5];

    for (~SYM[1] = 0; ~SYM[1] < ~SIZE[~TYP[6]]; ~SYM[1] = ~SYM[1] + 1) begin : ~GENSYM[IDDRX1F_GEN][2]
      IDDRX1F ~GENSYM[iddrx1f][3] (
        .D(~VAR[d][6][~SYM[1]]),
        .SCLK(~ARG[4]),
        .RST(~ARG[5]),
        .Q0(~SYM[4][~SYM[1]]),
        .Q1(~SYM[5][~SYM[1]])
      );

      assign ~RESULT = { ~SYM[4], ~SYM[5] };
    end
  ~FI
  // IDDRX1F end"
      }
    }
  ]
  |]
  )
  #-}
{-# NOINLINE iddrx1f #-}

-- | x1 output DDR
oddrx1f
  :: KnownConfiguration fast ('DomainConfiguration fast fPeriod edge reset init polarity) -- 0
  => KnownConfiguration slow ('DomainConfiguration slow (2 * fPeriod) edge reset init polarity) -- 1
  => NFDataX a -- 2
  => BitPack a -- 3
  => Clock slow -- 4

  -- ^ Clock
  -> Reset slow -- 5

  -- ^ Reset
  -> Signal slow a -- 6

  -- ^ Data to clock out on rising edge
  -> Signal slow a -- 7

  -- ^ Data to clock out on falling edge
  -> Signal fast a
  -- ^ Output
oddrx1f clk rst xs ys = ddrOut clk rst enableGen (unpack 0) ((,) <$> xs <*> ys)
{-# ANN
  oddrx1f
  ( InlinePrimitive [Verilog]
      $ unindent
        [i|
  [ { "BlackBox" :
      { "name"     : "Clash.Lattice.ECP5.Prims.oddrx1f"
      , "kind"     : "Declaration"
      , "template" :
  "// ODDRX1F begin
  ~IF~ISSCALAR[7]~THEN
    ODDRX1F ~GENSYM[oddrx1f][0] (
      .D0(~VAR[d0][6]),
      .D1(~VAR[d1][7]),
      .SCLK(~ARG[4]),
      .RST(~ARG[5]),
      .Q(~RESULT)
    );
  ~ELSE
    genvar ~GENSYM[i][1];
    for (~SYM[1] = 0; ~SYM[1] < ~SIZE[~TYP[6]]; ~SYM[1] = ~SYM[1] + 1) begin : ~GENSYM[ODDRX1F_GEN][2]
      ODDRX1F ~GENSYM[oddrx1f][3] (
        .D0(~VAR[d0][6][~SYM[1]]),
        .D1(~VAR[d1][7][~SYM[1]]),
        .SCLK(~ARG[4]),
        .RST(~ARG[5]),
        .Q(~RESULT[~SYM[1]])
      );
    end
  ~FI
  // ODDRX1F end"
      }
    }
  ]
  |]
  )
  #-}
{-# NOINLINE oddrx1f #-}

-- | Dynamic delay element
delayf
  :: delValue <= 127 -- 0
  => SNat delValue -- 1

  -- ^ Initial delay in 25ps increments
  -> Signal domLogic Bit -- 2

  -- ^ LOADN: 0 on this line resets to 0 delay setting
  -> Signal domLogic Bit -- 3

  -- ^ MOVE: Pulsing changes delay on falling edge according to DIRECTION
  -> Signal domLogic Bit -- 4

  -- ^ DIRECTION: 0 to increase delay by 25ps, 1 to decrease delay by 25ps
  -- Delay min is 0(0ps) max is 127(3175ps) and it's saturating.
  -> Signal domDelay a -- 5

  -- ^ Data input from pin or output register block
  -> Signal domDelay a
delayf !_ !_ !_ !_ inp = inp
{-# ANN
  delayf
  ( InlinePrimitive [Verilog]
      $ unindent
        [i|
  [ { "BlackBox" :
      { "name"     : "Clash.Lattice.ECP5.Prims.delayf"
      , "kind"     : "Declaration"
      , "template" :
  "// DELAYF begin
  ~IF~ISSCALAR[5]~THEN
    DELAYF #(
      .DEL_MODE(\\"USER_DEFINED\\"),
      .DEL_VALUE(7'd~LIT[1])
    ) ~GENSYM[DELAYF_INST][3] (
      .A(~VAR[d][5]),
      .LOADN(~VAR[loadn][2]),
      .MOVE(~VAR[move][3]),
      .DIRECTION(~VAR[direction][4]),
      .Z(~RESULT)
    );
  ~ELSE
    genvar ~GENSYM[i][0];
    for (~SYM[0] = 0; ~SYM[0] < ~SIZE[~TYP[5]]; ~SYM[0] = ~SYM[0] + 1) begin : ~GENSYM[DELAYF_GEN][1]
      DELAYF #(
        .DEL_MODE(\\"USER_DEFINED\\"),
        .DEL_VALUE(7'd~LIT[1])
      ) ~GENSYM[DELAYF_INST][2] (
        .A(~VAR[d][5][~SYM[0]]),
        .LOADN(~VAR[loadn][2]),
        .MOVE(~VAR[move][3]),
        .DIRECTION(~VAR[direction][4]),
        .Z(~RESULT[~SYM[0]])
      );
    end
  ~FI
  // DELAYF end"
      }
    }
  ]
  |]
  )
  #-}
{-# NOINLINE delayf #-}

-- | Static delay element
delayg
  :: delValue <= 127 -- 0
  => SNat delValue -- 1

  -- ^ Delay in 25ps increments
  -> Signal domDelay a -- 2

  -- ^ Data input from pin or output register block
  -> Signal domDelay a
delayg !_ inp = inp
{-# ANN
  delayg
  ( InlinePrimitive [Verilog]
      $ unindent
        [i|
  [ { "BlackBox" :
      { "name"     : "Clash.Lattice.ECP5.Prims.delayg"
      , "kind"     : "Declaration"
      , "template" :
  "// DELAYG begin
  ~IF~ISSCALAR[2]~THEN
    DELAYG #(
      .DEL_MODE(\\"USER_DEFINED\\"),
      .DEL_VALUE(7'd~LIT[1])
    ) ~GENSYM[DELAYG_INST][3] (
      .A(~VAR[d][2]),
      .Z(~RESULT)
    );
  ~ELSE
    genvar ~GENSYM[i][0];
    for (~SYM[0] = 0; ~SYM[0] < ~SIZE[~TYP[2]]; ~SYM[0] = ~SYM[0] + 1) begin : ~GENSYM[DELAYG_GEN][1]
      DELAYG #(
        .DEL_MODE(\\"USER_DEFINED\\"),
        .DEL_VALUE(7'd~LIT[1])
      ) ~GENSYM[DELAYG_INST][2] (
        .A(~VAR[d][2][~SYM[0]]),
        .Z(~RESULT[~SYM[0]])
      );
    end
  ~FI
  // DELAYG end"
      }
    }
  ]
  |]
  )
  #-}
{-# NOINLINE delayg #-}
