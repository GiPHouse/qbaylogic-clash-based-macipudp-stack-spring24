{-|
Module      : Clash.Signal.Extra
Description : Extra utility functions for working with signals
-}
module Clash.Signal.Extra
    ( registerN
    ) where

import Clash.Prelude

-- | a chain of registers of length n. So the delay from input -> output is n cycles.
registerN
  :: forall (dom :: Domain) (n :: Nat) (a :: Type)
   . HiddenClockResetEnable dom
  => NFDataX a
  => SNat n
  -- ^ The chain (or delay) length
  -> a
  -- ^ The initial value of the registers
  -> Signal dom a
  -> Signal dom a
registerN n@SNat initial inp = case compareSNat d1 n of
  SNatLE -> register initial $ registerN (SNat @(n - 1)) initial inp
  SNatGT -> inp
