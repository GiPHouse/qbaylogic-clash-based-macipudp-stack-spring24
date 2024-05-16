module Clash.Sized.Vector.Extra (
    takeLe
  , appendVec
  , foldPipeline
  , PipelineLatency
) where

import Clash.Prelude
import Data.Proxy
import Data.Type.Equality
-- | Like 'take' but uses a 'Data.Type.Ord.<=' constraint
takeLe
  :: forall (n :: Nat)
            (m :: Nat)
            a
   . n <= m
  => SNat n
  -- ^ How many elements to take
  -> Vec m a
  -- ^ input vector
  -> Vec n a
takeLe SNat vs = leToPlus @n @m $ takeI vs

-- | Take the first 'valid' elements of 'xs', append 'ys', then pad with 0s
appendVec
  :: forall n m a
   . KnownNat n
  => Num a
  => Index n
  -> Vec n a
  -> Vec m a
  -> Vec (n + m) a
appendVec valid xs ys = results !! valid
  where
    go :: forall l. SNat l -> Vec (n + m) a
    go l@SNat = let f = addSNat l d1 in case compareSNat f (SNat @n) of
                  SNatLE -> takeLe (addSNat l d1) xs ++ ys ++ extra
                   where
                     extra :: Vec (n - (l + 1)) a
                     extra = repeat 0
                  _ -> error "appendVec: Absurd"
    results = smap (\s _ -> go s) xs

-- | Fold a vector of 'n' elements into a single element using a binary function.
-- | Between every "layer" of the fold, there is a register
-- | This means there is a latency between input and output of 'PipelineDelay width' cycles.
-- | This is equal to CLog2(width) + 1
foldPipeline ::
  forall (dom :: Domain) (n :: Nat) (a :: Type).
  HiddenClockResetEnable dom
  => KnownNat n
  => 1 <= n
  => NFDataX a
  => a
  -> (a -> a -> a)
  -- ^ Associative binary operation to apply
  -> Signal dom (Vec n a)
  -- ^ Input values
  -> Signal dom a
foldPipeline initial f inp  = case (nIs1, foldWidthBiggerThan1) of
    (_, SNatGT) -> error "n `Div` 2 + n `Mod` 2 <= 1 impossible"
    (Just Refl, _) -> head <$> inp
    (Nothing, SNatLE) -> foldPipeline initial f foldValues
    where
      nIs1 = sameNat (Proxy :: Proxy n) (Proxy :: Proxy 1)
      foldWidthBiggerThan1 = compareSNat d1 (SNat @(n `Div` 2 + n `Mod` 2))

      foldValues :: Signal dom (Vec (n `Div` 2 + n `Mod` 2) a)
      foldValues =
        case (atLeast1mod2, nEqualsN) of
        (SNatLE, Just Refl) -> (step @(n `Div` 2) @(n `Mod` 2)) (Proxy::Proxy (n `Div` 2)) initial f inp
        _ -> error "'n % 2 > 1', or '2*(n/2)+n%2 != x': impossible"

      atLeast1mod2 = compareSNat (SNat @(n `Mod` 2)) d1
      nEqualsN = sameNat (Proxy :: Proxy (2 * (n `Div` 2) + n `Mod` 2)) (Proxy :: Proxy n)

step :: forall (m :: Nat) (p :: Nat) (dom :: Domain) (a :: Type).
  HiddenClockResetEnable dom
  => KnownNat m
  => KnownNat p
  => p <= 1
  => NFDataX a
  => Proxy m
  -> a
  -> (a -> a -> a)
  -> Signal dom (Vec (2 * m + p) a)
  -> Signal dom (Vec (m + p) a)
step _ initial f inps = case (
    sameNat (Proxy :: Proxy p) (Proxy :: Proxy 0),
    sameNat (Proxy :: Proxy p) (Proxy :: Proxy 1)
  ) of
  (Just Refl, Nothing) -> regVec $ layerCalc inps
  (Nothing, Just Refl) -> regVec $ (++) <$> (singleton . head <$> inps) <*> layerCalc (tail <$> inps)
  _ -> error "p > 1 impossible"
  where
    layerCalc :: Signal dom (Vec (2*m) a) -> Signal dom (Vec m a)
    layerCalc = fmap (fmap applyF . unconcatI)

    applyF :: Vec 2 a -> a
    applyF (a :> b :> _) = f a b
    applyF _ = error "calcChecksum2: impossible"

    regVec :: KnownNat q => Signal dom (Vec q a) -> Signal dom (Vec q a)
    regVec vs = bundle $ register initial <$> unbundle vs

-- | The latency of the pipeline
type PipelineLatency (n :: Nat) = 1 + CLog 2 n
