module Clash.Cores.Ethernet.Converters
  ( upConverter
  , sampleOut
  ) where

import Clash.Prelude

data UpConverterState (p :: Nat) (n :: Nat) =
  UpConverterState {
    _buffer      :: Vec p (BitVector n),
    _byteEnable  :: Vec p Bool,
    _flushEarly  :: Bool,
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
  )
-- _dataOut:     Upconverted stream. Is Nothing unless _consumer_ready && one upconverted word is ready
-- _byteEnable:  If Nothing, all bytes in upconverted stream are enabled by default, otherwise signifies which bytes are enabled.
-- _abort:        Error occurred
-- _receiveReady: The upconverter is ready to receive a word

upConverter dataIn0 abort0 consumerReady0 = mealyB go s_0 (dataIn0, abort0, consumerReady0)
    where
      go
        :: UpConverterState p n
        -> (Maybe (Bool, BitVector n), Bool, Bool)
        -> (UpConverterState p n, (Maybe(Vec p (BitVector n)), Maybe(Vec p Bool), Bool, Bool))

      go st@(UpConverterState buffer byteEnable flushEarly abortRaised) (dataIn1, abort1, consReady1) =
        case dataIn1 of
          Nothing -> (st, (Nothing, Nothing, abortRaised, True))
          Just (lastByte, word) ->
            let isFull  = last @(p-1) byteEnable
                flush = isFull || flushEarly

                flushEarlyNext = lastByte

                bufferNext = buffer <<+ word
                byteEnableNext = if flush then repeat False <<+ True else byteEnable <<+ False -- reset byte-enable vector after flushing

                canWrite = (not isFull) || consReady1 -- If we are not full, or full but consumer can consume the stream, we can receive new data next cycle
                
                abortRaisedNext = (abortRaised || abort1) && (not flush) -- reset abort of state after flushing

                outStream = if flush then Just buffer else Nothing -- outstream only if flush
                
            in (UpConverterState 
                  bufferNext 
                  byteEnableNext 
                  flushEarlyNext 
                  abortRaisedNext, 
                  (outStream, if isFull then Nothing else Just byteEnable, abortRaised, canWrite))

      s_0 = UpConverterState (repeat 0) (repeat False) False False

payloadInp :: [Maybe (Bool, BitVector 1)]
payloadInp = [
    Just (False,0b0 :: BitVector 1)
  , Just (False,0b0 :: BitVector 1)
  , Just (False,0b0 :: BitVector 1)
  , Just (False,0b0 :: BitVector 1)
  
  , Just (False,0b0 :: BitVector 1)
  , Just (False,0b0 :: BitVector 1)
  , Just (True,0b0 :: BitVector 1)
  , Just (True,0b0 :: BitVector 1)]
abortInp :: [Bool]
abortInp = [False, False, True, False, False, True, False, False]

sinkReadyInp :: [Bool]
sinkReadyInp = [False, True, True, True, False, False, False, True]

clk :: Clock System
clk = systemClockGen

rst :: Reset System
rst = systemResetGen

en :: Enable dom
en = enableGen

payloadOut :: "_dataOut" ::: Signal System (Maybe (Vec 4 (BitVector 1)))
byteEnOut :: "_byteEnable" ::: Signal System (Maybe (Vec 4 Bool))
abortOut :: "_abort" ::: Signal System Bool
sinkReadyOut :: "_receiveReady" ::: Signal System Bool
(payloadOut, byteEnOut, abortOut, sinkReadyOut) = exposeClockResetEnable (upConverter @4) clk rst en (fromList payloadInp) (fromList abortInp) (fromList sinkReadyInp)

sampleOut :: [(Maybe (Vec 4 (BitVector 1)), Maybe(Vec 4 Bool), Bool, Bool)]
sampleOut = sampleN 60 $ bundle (payloadOut, byteEnOut, abortOut, sinkReadyOut)
