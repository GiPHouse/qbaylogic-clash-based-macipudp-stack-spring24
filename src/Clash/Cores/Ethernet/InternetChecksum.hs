module Clash.Cores.Ethernet.InternetChecksum
  ( internetChecksum
  ) where

import Clash.Prelude
import Data.Maybe

-- | computes the internet checksum of a stream of 16-bit words according to https://datatracker.ietf.org/doc/html/rfc1071
-- keep in mind that the checksum is updated with the input data in the same clock cycle, while the reset is delayed by one cycle.
internetChecksum
  :: forall (dom :: Domain).
  HiddenClockResetEnable dom
  => Signal dom (Maybe (BitVector 16, Bool))
  -- ^ Input data, adds the first data point of the checksum, if the second element of the tuple is True, the current checksum is reset to 0 the next cycle
  -> Signal dom (BitVector 16)
 -- ^ Resulting checksum
internetChecksum inputM = complement <$> checkSumWithCarry
  where
    (inpX, resetX) = unbundle $ fromJustX <$> inputM

    checkSum :: Signal dom (BitVector 17)
    checkSum = regEn 0 (isJust <$> inputM) $ mux resetX 0 nextCheckSum

    (fmap zeroExtend -> carry, truncated) = unbundle $ split <$> checkSum
    checkSumWithCarry = carry + truncated
    nextCheckSum = add <$> inpX <*> checkSumWithCarry
