{-|
Module      : Clash.Cores.Ethernet.InternetChecksum
Description : Functions for computing the RFC1071 internet checksum
-}


module Clash.Cores.Ethernet.InternetChecksum
  ( internetChecksum,
    reduceToInternetChecksum,
    pipelinedInternetChecksum,
    PipeLineDelay
  ) where

import Clash.Prelude
import Data.Maybe
import Data.Proxy
import Data.Type.Equality

-- | computes the un-complimented internet checksum of a stream of 16-bit words according to https://datatracker.ietf.org/doc/html/rfc1071
-- The checksum and reset are delayed by one clock cycle.
-- Keep in mind that if "reset" is True in the input tuple, the checksum is reset to 0 the next cycle so the value of the bitvector is disgarded
internetChecksum
  :: forall (dom :: Domain).
  HiddenClockResetEnable dom
  => Signal dom (Maybe (BitVector 16, Bool))
  -- ^ Input data, adds the first data point of the checksum, if the second element of the tuple is True, the current checksum is reset to 0 the next cycle
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

-- | Computes the internetChecksum of a vector of 16 bit words. Compared to internetChecksum this is quicker as you can load multiple words per cycle.
reduceToInternetChecksum ::
  forall (dom :: Domain) (width :: Nat).
  HiddenClockResetEnable dom
  => 1 <= width
  => Signal dom (Maybe (Vec width (BitVector 16), Bool))
  -- ^ Input data, adds the first data point of the checksum, if the second element of the tuple is True, the current checksum is reset to 0 the next cycle
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
  -- ^ Input data, adds the first data point of the checksum, if the second element of the tuple is True, the current checksum is reset to 0 the next cycle
  -> Signal dom (BitVector 16)
  -- ^ Resulting checksum
pipelinedInternetChecksum inputM = checkSum
  where
    checkSum = register 0 $ mux reset 0 checksumResult
    (inp, reset) = unbundle $ fromMaybe (repeat 0, False) <$> inputM
    checksumResult = calcChecksum <$> foldChecksum inp <*> checkSum

foldChecksum ::
  forall (dom :: Domain) (n::Nat).
  HiddenClockResetEnable dom =>
  KnownNat n
  => Signal dom ((Vec n) (BitVector 16))
  -> Signal dom (BitVector 16)
foldChecksum inp = case sameNat (Proxy :: Proxy n ) (Proxy :: Proxy 1) of
    Just Refl -> head <$> inp
    Nothing -> foldChecksum foldValues
      where
        foldValues :: Signal dom ((Vec (n `Div` 2 + n `Mod` 2)) (BitVector 16))
        foldValues = case (
              compareSNat (SNat @(n `Mod` 2)) d1,
              sameNat (Proxy :: Proxy (2 * (n `Div` 2) + n `Mod` 2)) (Proxy:: Proxy n)
            ) of
          (SNatLE, Just Refl) -> (step @(n `Div` 2) @(n `Mod` 2)) (Proxy::Proxy (n `Div` 2))inp
          _ -> error "'n % 2 > 1', or '2*(n/2)+n%2 != x': impossible"

        step ::
          forall (m::Nat) (p::Nat).
          KnownNat m
          => KnownNat p
          => p <= 1
          => Proxy m
          -> Signal dom (Vec (2*m+p) (BitVector 16))
          -> Signal dom (Vec (m+p) (BitVector 16))
        step _ inps = case (
            sameNat (Proxy :: Proxy p) (Proxy :: Proxy 0),
            sameNat (Proxy :: Proxy p) (Proxy :: Proxy 1)
          ) of
          (Just Refl, Nothing) -> layerCalc inps
          (Nothing, Just Refl) -> (++) <$> register (repeat 0) (singleton . head <$> inps) <*> layerCalc (tail <$> inps)
          _ -> error "p > 1 impossible"
          where
            layerCalc :: Signal dom (Vec (2*m) (BitVector 16)) -> Signal dom (Vec m (BitVector 16))
            layerCalc inl = register (repeat 0) $ fmap (fmap calcChecksum2 . unconcatI) inl

            calcChecksum2 :: Vec 2 (BitVector 16) -> BitVector 16
            calcChecksum2 (a :> b :> _) = calcChecksum a b
            calcChecksum2 _ = error "calcChecksum2: impossible"

-- -- Define a type to determine the needed delay
type PipeLineDelay (n :: Nat) = 1 + CLog 2 n
