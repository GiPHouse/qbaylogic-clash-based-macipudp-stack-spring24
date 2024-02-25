module Clash.Cores.Ethernet.Converters
  ( upConverter
  , sampleOut
  ) where

import Clash.Prelude
import Data.Maybe

data UpConverterState (p :: Nat) (n :: Nat) =
  FillNext {
    _buffer      :: Vec p (BitVector n),
    _byteEnable  :: Vec p Bool,
    _abortRaised :: Bool
  }
  |
  FlushNext {
    _buffer      :: Vec p (BitVector n),
    _byteEnable  :: Vec p Bool,
    _abortRaised :: Bool
  }
  deriving (Generic, NFDataX)
-- ^ Upconverter state, consisting of at most p (BitVector n)s and a vector indicating which bytes are valid                                            

upConverter
  :: forall (p :: Nat) (n :: Nat) (dom :: Domain).
  HiddenClockResetEnable dom
  => 1 <= n
  => 1 <= p
  => "_wordSize" ::: KnownNat n
  => "_upconvStreamSize" ::: KnownNat p
  => "_dataIn" ::: Signal dom (Maybe (Bool, BitVector n))
  -- ^ data stream, The bool indicates whether this is the last word of a packet
  -> "_abort" ::: Signal dom Bool
  -- ^ Error occured
  -> "_consumerReady" ::: Signal dom Bool
  -- ^ The consumer is ready to receive a word
  -> ( "_dataOut"      ::: Signal dom (Maybe (Vec p (BitVector n)))
     , "_byteEnable"   ::: Signal dom (Maybe (Vec p Bool))
     , "_abort"         ::: Signal dom Bool
     , "_receiveReady" ::: Signal dom Bool

-- _dataOut:     Upconverted stream. Is Nothing unless _consumer_ready && one upconverted word is ready
-- _byteEnable:  If Nothing, all bytes in upconverted stream are enabled by default, otherwise signifies which bytes are enabled.
-- _abort:        Error occurred
-- _receiveReady: The upconverter is ready to receive a word
     )

upConverter dataIn0 abort0 consumerReady0 = mealyB go s_0 (dataIn0, abort0, consumerReady0)
    where
      go
        :: UpConverterState p n
        -> (Maybe (Bool, BitVector n), Bool, Bool)
        -> (UpConverterState p n, (Maybe(Vec p (BitVector n)), Maybe(Vec p Bool), Bool, Bool))

      go st@(UpConverterState buffer byteEnable abortRaised) (dataIn1, abort1, consReady1) =
        case dataIn1 of
          Nothing -> (st, (Nothing, Nothing, abortRaised, True))
          Just (lastByte, word) ->
            let isEmpty = head @(p-1) byteEnable
                full  = last @(p-1) byteEnable
                flush = full || lastByte

                bufferNext = buffer <<+ word
                byteEnableNext = if isEmpty then repeat 0 <<+ 1 else buffer <<+ 0

                canWrite = (not full) || consReady1 -- If we are not full, or full but consumer can consume the stream, we can receive new data next cycle
                
                abortRaisedNext = (abortRaised || abort1) && (not isEmpty)

                outStream = if flush then Just buffer else Nothing
                
            in (UpConverterState bufferNext byteEnableNext abortRaisedNext, outStream canWrite)

      s_0 = UpConverterState (repeat 0) 0 (repeat 0)

payloadInp :: [Maybe (Bool, BitVector 8)]
payloadInp = [Nothing]

abortInp :: [Bool]
abortInp = [False]

sinkReadyInp :: [Bool]
sinkReadyInp = [False]

clk :: Clock System
clk = systemClockGen

rst :: Reset System
rst = systemResetGen

en :: Enable dom
en = enableGen

payloadOut :: "data out, valid byte" ::: Signal System (Maybe (Vec 4 (BitVector 8), BitVector 4))
abortOut :: "error out" ::: Signal System Bool
sinkReadyOut :: "receive ready" ::: Signal System Bool
(payloadOut, abortOut, sinkReadyOut) = exposeClockResetEnable (upConverter @4) clk rst en (fromList payloadInp) (fromList abortInp) (fromList sinkReadyInp)

sampleOut :: [(Maybe (Vec 4 (BitVector 8), BitVector 4), Bool, Bool)]
sampleOut = sampleN 60 $ bundle (payloadOut, abortOut, sinkReadyOut)
