{-|
Module      : Clash.Cores.Ethernet.Util
Description : Utility module, only for very small util functions
-}
module Clash.Cores.Ethernet.Util
    ( toMaybe
    , registerN
    ) where

import Clash.Prelude

-- | Wrap a value in a Just if True
toMaybe :: Bool -> a -> Maybe a
toMaybe True x = Just x
toMaybe False _ = Nothing

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
