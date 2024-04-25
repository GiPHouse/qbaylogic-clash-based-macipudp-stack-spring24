{-|
Module      : Clash.Cores.Ethernet.InternetChecksum
Description : Functions for computing the RFC1071 internet checksum
-}
module Clash.Cores.Ethernet.InternetChecksum
  ( internetChecksum
  ) where

import Clash.Prelude
import Data.Maybe

-- | Computes the uncomplimented internet checksum of a stream of 16-bit words
-- according to https://datatracker.ietf.org/doc/html/rfc1071. The checksum and
-- reset are delayed by one clock cycle. Keep in mind that if "reset" is True in
-- the input tuple, the checksum is reset to 0 the next cycle so the value of
-- the `BitVector` is disgarded.
internetChecksum
  :: forall (dom :: Domain) (dataWidth :: Nat).
  HiddenClockResetEnable dom
  => KnownNat dataWidth
  => 1 <= dataWidth
  => Signal dom (Maybe (BitVector dataWidth, Bool))
  -- ^ Input data, adds the first data point of the checksum, if the second element of the tuple is True, the current checksum is reset to 0 the next cycle
  -> Signal dom (BitVector dataWidth)
 -- ^ Resulting checksum
internetChecksum inputM = checkSumWithCarry
  where
    (inpX, resetX) = unbundle $ fromJustX <$> inputM

    checkSum :: Signal dom (BitVector (dataWidth + 1))
    checkSum = regEn 0 (isJust <$> inputM) $ mux resetX 0 nextCheckSum

    (fmap (zeroExtend :: BitVector 1 -> BitVector ((dataWidth - 1) + 1)) -> carry, truncated) = unbundle $ split <$> checkSum

    checkSumWithCarry = carry + truncated
    nextCheckSum = add <$> inpX <*> checkSumWithCarry
