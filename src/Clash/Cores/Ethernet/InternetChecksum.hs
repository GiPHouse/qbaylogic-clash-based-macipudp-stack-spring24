{-|
Module      : Clash.Cores.Ethernet.InternetChecksum
Description : Functions for computing the RFC1071 internet checksum
-}
module Clash.Cores.Ethernet.InternetChecksum
  ( internetChecksum,
    reduceToInternetChecksum,
    pipelinedInternetChecksum,
    PipelineDelay
  ) where

import Clash.Cores.Ethernet.Util qualified as U
import Clash.Prelude
import Data.Maybe
import Data.Proxy
import Data.Type.Equality

-- | computes the un-complimented internet checksum of a stream of 16-bit words
-- according to https://datatracker.ietf.org/doc/html/rfc1071
-- The checksum and reset are delayed by one clock cycle.
-- Keep in mind that if "reset" is True in the input tuple, the checksum is
-- reset to 0 the next cycle so the value of the bitvector is disgarded
internetChecksum
  :: forall (dom :: Domain).
  HiddenClockResetEnable dom
  => Signal dom (Maybe (BitVector 16, Bool))
  -- ^ Input data, adds the first data point of the checksum, if the second
  -- element of the tuple is True, the current checksum is reset to 0 the next cycle
  -> Signal dom (BitVector 16)
 -- ^ Resulting checksum
internetChecksum inputM = checkSumWithCarry
  where
    (inpX, resetX) = unbundle $ fromJustX <$> inputM

    checkSum :: Signal dom (BitVector 17)
    checkSum = regEn 0 (isJust <$> inputM) $ mux resetX 0 nextCheckSum

    (fmap zeroExtend -> carry, truncated) = unbundle $ split <$> checkSum

    checkSumWithCarry = carry + truncated
    nextCheckSum = add <$> inpX <*> checkSumWithCarry

calcChecksum :: BitVector 16 -> BitVector 16 -> BitVector 16
calcChecksum bvA bvB = carry + truncated
  where
    (zeroExtend -> carry, truncated) = split checkSum
    checkSum :: BitVector 17
    checkSum = add bvA bvB

-- | Computes the internetChecksum of a vector of 16 bit words. Compared to
-- internetChecksum this is quicker as you can load multiple words per cycle
reduceToInternetChecksum ::
  forall (dom :: Domain) (width :: Nat).
  HiddenClockResetEnable dom
  => 1 <= width
  => Signal dom (Maybe (Vec width (BitVector 16), Bool))
  -- ^ Input data, adds the first data point of the checksum, if the second
  -- element of the tuple is True, the current checksum is reset to 0 the next cycle
  -> Signal dom (BitVector 16)
  -- ^ Resulting checksum
reduceToInternetChecksum inputM = checkSum
  where
    checkSum = regEn 0 (isJust <$> inputM) $ mux resetX 0 checksumResult
    (inpX, resetX) = unbundle $ fromJustX <$> inputM
    checksumResult = fold calcChecksum <$> input
    input = (++) <$> (singleton <$> checkSum) <*> inpX

pipelinedInternetChecksum ::
  forall (dom :: Domain) (width :: Nat).
  HiddenClockResetEnable dom
  => 1 <= width
  => KnownNat width
  => Signal dom (Maybe (Vec width (BitVector 16), Bool))
  -- ^ Input data, adds the first data point of the checksum, if the second
  -- element of the tuple is True, the current checksum is reset to 0 the next cycle
  -> Signal dom (BitVector 16)
  -- ^ Resulting checksum
pipelinedInternetChecksum inputM = checkSum
  where
    checkSum = register 0 $ mux reset 0 checksumResult
    (inp, resetInp) = unbundle $ fromMaybe (repeat 0, False) <$> inputM
    checksumResult = calcChecksum <$> foldPipeline 0 calcChecksum inp <*> checkSum
    reset = U.registerN (SNat :: SNat (PipelineDelay width-1)) False resetInp


foldPipeline ::
  forall (dom :: Domain) (n::Nat) (a::Type).
  HiddenClockResetEnable dom
  => KnownNat n
  => 1 <= n
  => NFDataX a
  => a
  -> (a -> a -> a)
  -> Signal dom (Vec n a)
  -> Signal dom a
foldPipeline initial func inp  = case (
    sameNat (Proxy :: Proxy n) (Proxy :: Proxy 1),
    compareSNat d1 (SNat @(n `Div` 2 + n `Mod` 2))
    ) of
    (_, SNatGT) -> error "n `Div` 2 + n `Mod` 2 <= 1 impossible"
    (Just Refl, _) -> head <$> inp
    (Nothing, SNatLE) -> foldPipeline initial func foldValues
      where
        foldValues :: Signal dom (Vec (n `Div` 2 + n `Mod` 2) a)
        foldValues =
          case (
              compareSNat (SNat @(n `Mod` 2)) d1,
              sameNat (Proxy :: Proxy (2 * (n `Div` 2) + n `Mod` 2)) (Proxy:: Proxy n)
            ) of
          (SNatLE, Just Refl) -> (step @(n `Div` 2) @(n `Mod` 2)) (Proxy::Proxy (n `Div` 2)) inp
          _ -> error "'n % 2 > 1', or '2*(n/2)+n%2 != x': impossible"

        step ::
          forall (m::Nat) (p::Nat).
          KnownNat m
          => KnownNat p
          => p <= 1
          => Proxy m
          -> Signal dom (Vec (2 * m + p) a)
          -> Signal dom (Vec (m + p) a)
        step _ inps = case (
            sameNat (Proxy :: Proxy p) (Proxy :: Proxy 0),
            sameNat (Proxy :: Proxy p) (Proxy :: Proxy 1)
          ) of
          (Just Refl, Nothing) -> layerCalc inps
          (Nothing, Just Refl) -> (++) <$> register (repeat initial) (singleton . head <$> inps) <*> layerCalc (tail <$> inps)
          _ -> error "p > 1 impossible"
          where
            layerCalc :: Signal dom (Vec (2*m) a) -> Signal dom (Vec m a)
            layerCalc inl = register (repeat initial) $ fmap (fmap calcChecksum2 . unconcatI) inl

            calcChecksum2 :: Vec 2 a -> a
            calcChecksum2 (a :> b :> _) = func a b
            calcChecksum2 _ = error "calcChecksum2: impossible"

-- Define a type to determine the needed delay
type PipelineDelay (n :: Nat) = 1 + CLog 2 n
