{-# language MagicHash #-}


module Clash.Cores.Ethernet.InternetChecksum
  ( internetChecksum
  ) where

import Clash.Prelude
import Clash.Sized.Internal.BitVector ( complement# )
import Data.Maybe ( fromMaybe )

-- | computes the internet checksum of a stream of 16-bit words according to https://datatracker.ietf.org/doc/html/rfc1071
internetChecksum
  :: forall (dom :: Domain).
  HiddenClockResetEnable dom
  => Signal dom (Maybe (BitVector 16, Bool))
  -- ^ Input  data, if the second element of the tuple is True, the current checksum is reset to 0 the next cycle
  -> Signal dom (BitVector 16)
 -- ^ Resulting checksum
internetChecksum input = complement# <$> checkSum'
  where
    checkSum = register 0 $ mux reset 0 checkSum'

    checkSum' :: Signal dom (BitVector 16)
    checkSum' = checkSum + (fst <$> sumVal) + carry
     where
        sumVal = calcSum <$> checkSum <*> inputValue
        carry =  zeroExtend . pack . snd <$> sumVal

    calcSum :: BitVector 16 -> BitVector 16 -> (BitVector 16, Bit)
    calcSum a b = (truncateB sum17, msb sum17)
      where
        sum17 :: BitVector 17
        sum17  = zeroExtend a + zeroExtend b

    reset = maybe False snd <$> input
    inputValue = maybe 0 fst <$> input
