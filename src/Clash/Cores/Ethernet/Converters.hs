module Clash.Cores.Ethernet.Converters
  ( upConverter
  ) where

import Clash.Prelude
import Data.Maybe

data UpConverterSt (p :: Nat) (n :: Nat) = UpConverterSt (Vec p (BitVector n)) (Index (p + 1))
  deriving (Generic, NFDataX)
-- ^ Up converter state, consisting of at most p (BitVector n)s and the number of of element

upConverter
  :: forall (p :: Nat) (n :: Nat) (dom :: Domain).
  HiddenClockResetEnable dom
  => 1 <= n
  => 1 <= p
  => KnownNat n
  => KnownNat p
  => Signal dom (Maybe (Bool, BitVector n))
  -- ^ data stream, The bool indicates whether this is the last word of a packet
  -> Signal dom Bool
  -- ^ Error occured
  -> Signal dom Bool
  -- ^ The consumer is ready to receive a word
  -> ( Signal dom (Maybe (Bool, Vec p (BitVector n)))
     , Signal dom Bool
     , Signal dom Bool
     )
   -- ^ The upconverted stream. if the bool (last) is true then the index indicates
   --   how many words are valid in the vec. E.g. If last && index == 0. then
   --   it means only the first element in the vec contains a valid word
   --
   -- The second element is the error
   --
   -- The third element is whether the upConverter is ready to receive data.
upConverter payload abort sinkRdy = case compareSNat (SNat @n) (SNat @1) of
  SNatLE -> (fmap (fmap (fmap Clash.Prelude.repeat)) payload, abort, sinkRdy)
  _ -> mealyB go s_0 (payload, abort, sinkRdy)
    where
      go
        :: UpConverterSt p n 
        -> (Maybe (Bool, BitVector n), Bool, Bool)
        -> (UpConverterSt p n, (Maybe (Bool, Vec p (BitVector n)), Bool, Bool))

      go (UpConverterSt xs i) (payload', abort', sinkRdy') =
        let notFull = i < natToNum @p
            canWrite = notFull || sinkRdy'
            (xs', add_i, last_be') = case (canWrite, payload') of
              (True, Just (last_be, x)) -> (xs <<+ x, 1, last_be)
              _ -> (xs, 0, False)
            payload'' = if notFull || not last_be'
              then Nothing
              else Just (last_be', xs')
            sub_i = if isJust payload''
              then natToNum @n
              else 0
            i' = (i - sub_i) + add_i
        in (UpConverterSt xs' i', (payload'', abort', canWrite))

      s_0 = UpConverterSt (repeat 0) 0
