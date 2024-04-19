{-# language RecordWildCards #-}
module Clash.Cores.Ethernet.PadPacket
  ( padPacketC
  ) where

import Clash.Cores.Ethernet.PacketStream
import Clash.Prelude
import Data.List qualified as L
import Data.Maybe
import Protocols ( Circuit, fromSignals )

-- | State of the padPacket circuit.
-- Counts up to ceil(64/dataWidth) packets, which is the required
-- amount to fulfill the minimum ethernet frame size of 64.
data PadPacketState (dataWidth :: Nat)
  = Filling { cycles :: Index (Div (63 + dataWidth) dataWidth)}
  | Full
  | Padding { cycles :: Index (Div (63 + dataWidth) dataWidth)}
  deriving (Eq, Show, Generic, NFDataX)

padPacket
  :: forall (dataWidth :: Nat) (dom :: Domain).
  HiddenClockResetEnable dom
  => 1 <= dataWidth
  => KnownNat dataWidth
  => ( Signal dom (Maybe (PacketStreamM2S dataWidth ()))
     , Signal dom PacketStreamS2M)
  -- ^ Input packet stream from the source
  --   Input backpressure from the sink
  -> ( Signal dom PacketStreamS2M
     , Signal dom (Maybe (PacketStreamM2S dataWidth ()))
     )
  -- ^ Output backpressure to the source
  --   Output packet stream to the sink
padPacket = mealyB go s0
  where
    s0 = Filling 0
    go
      :: PadPacketState dataWidth
      -> (Maybe (PacketStreamM2S dataWidth ()), PacketStreamS2M)
      -> (PadPacketState dataWidth, (PacketStreamS2M, Maybe (PacketStreamM2S dataWidth ())))
    go st (fwdIn, PacketStreamS2M inReady)
      = (st', (bwdOut, fwdOut))
      where
        st' = if isJust fwdOut && not inReady then st else case (st, fwdIn) of
          -- If we are filling, then the next state depends on _last
          -- If _last is Nothing and we fill the 64 bytes, then go to Full state, otherwise keep filling
          -- If _last is Just, then go to Padding state if not full, otherwise go to s0
          (Filling n, Just PacketStreamM2S {..}) -> case _last of
            Nothing -> if n < maxBound then Filling (n + 1) else Full
            Just _  -> if n < maxBound then Padding (n + 1) else s0
          -- If we are filling and don't receive any input, we keep filling
          (Filling _, Nothing) -> st
          -- If we are full, then we stay in Full state until _last is Just
          (Full, Just PacketStreamM2S {..}) -> case _last of
            Nothing -> st
            Just _  -> s0
          -- If we are full and don't receive any input, stay full
          (Full, Nothing) -> st
          -- While padding, it does not matter if we receive input
          (Padding n, _) -> if n < maxBound then Padding (n + 1) else s0

        readyOut = case st of
          Padding _ -> False
          _         -> isJust fwdOut && inReady
        bwdOut = PacketStreamS2M {_ready = readyOut}

        calcMod = mod (63 :: Index 64) (natToNum @dataWidth)
        lastOut = case st of
          Filling n -> if n == maxBound && st' == s0 then Just (max (fromMaybe 0 (_last (fromJust fwdIn))) (resize calcMod)) else Nothing
          Full      -> _last =<< fwdIn
          Padding n -> if n == maxBound && st' == s0 then Just (resize calcMod) else Nothing
        -- lastOut = case (st, fwdIn) of
        --   (Filling n, Just PacketStreamM2S {..}) -> if st' == s0 then Just (resize (maxBound - n)) else Nothing
        --   (Full, Just PacketStreamM2S {..}) -> if st' == s0 then _last else Nothing
        --   (Padding n, _) -> if st' == s0 then Just (resize (n-1)) else Nothing
        --   _ -> Nothing
        fwdOut = case st of
          Padding _ -> Just PacketStreamM2S{_data = repeat 0, _last = lastOut, _meta = (), _abort = False}
          _         -> if isJust fwdIn then Just (fromJust fwdIn){_last = lastOut} else fwdIn
        -- fwdOut = case (st, fwdIn) of
        --   (Filling _, Just PacketStreamM2S {..}) -> Just PacketStreamM2S{_data = _data, _last = lastOut, _meta = _meta, _abort = _abort}
        --   (Full, Just PacketStreamM2S {..}) -> Just PacketStreamM2S{_data = _data, _last = lastOut, _meta = _meta, _abort = _abort}
        --   (Padding _, _) -> Just PacketStreamM2S{_data = repeat 0, _last = lastOut, _meta = (), _abort = False}
        -- _ -> Nothing

        -- fwdOut (Filling n) (Just PacketStreamM2S {..}) _ = if st' == s0 then Nothing else Nothing
        -- fwdOut (Filling n) Nothing _ = Nothing
        -- fwdOut Full (Just PacketStreamM2S {..}) _ = undefined

padPacketC
  :: forall (dataWidth :: Nat) (dom :: Domain).
  HiddenClockResetEnable dom
  => 1 <= dataWidth
  => KnownNat dataWidth
  => Circuit (PacketStream dom dataWidth ()) (PacketStream dom dataWidth ())
padPacketC = fromSignals padPacket

payloadInp :: [Maybe (PacketStreamM2S 4 ())]
payloadInp = [
  Nothing
  , Just (PacketStreamM2S (0x01 :> 0x02 :> 0x03 :> 0x04 :> Nil) (Just 1) () False)
  , Nothing
  -- , Nothing
  -- , Nothing
  -- , Nothing
  -- , Nothing
  -- , Nothing
  , Just (PacketStreamM2S (0xAA :> 0xFF :> 0xFF :> 0xFF :> Nil) (Nothing) () False)
  -- , Nothing
  -- , Nothing
  , Just (PacketStreamM2S (0xDE :> 0xAD :> 0x00 :> 0x00 :> Nil) (Just 0) () True)
  -- , Just (PacketStreamM2S (0xDE :> 0xAD :> 0x00 :> 0x00 :> Nil) (Just 1) () True)
  -- , Nothing
  -- , Nothing
  -- , Just (PacketStreamM2S (0xAA :> 0xAA :> 0xAA :> 0xAA :> Nil) Nothing () False)
  -- , Just (PacketStreamM2S (0xAA :> 0xAA :> 0xAA :> 0xAA :> Nil) Nothing () False)
  -- , Just (PacketStreamM2S (0x55 :> 0x55 :> 0x55 :> 0x55 :> Nil) (Just 3) () False)
  -- , Just (PacketStreamM2S (0x55 :> 0x55 :> 0x55 :> 0x55 :> Nil) (Just 3) () False)
  -- , Just (PacketStreamM2S (0x55 :> 0x55 :> 0x55 :> 0x55 :> Nil) (Just 3) () False)
  -- , Just (PacketStreamM2S (0x55 :> 0x55 :> 0x55 :> 0x55 :> Nil) (Just 3) () False)
  ] L.++ L.repeat Nothing

sinkReadyInp :: [PacketStreamS2M]
sinkReadyInp = fmap PacketStreamS2M ([False, True, True, True, True, True, True] L.++ (L.repeat True))

clk :: Clock System
clk = systemClockGen

rst :: Reset System
rst = systemResetGen

en :: Enable dom
en = enableGen

payloadOut :: Signal System (Maybe (PacketStreamM2S 4 ()))
sinkReadyOut :: Signal System PacketStreamS2M
padderClk = exposeClockResetEnable padPacket clk rst en

(sinkReadyOut, payloadOut) = padderClk (fromList payloadInp, fromList sinkReadyInp)

sampleOut = sampleN 12 $ bundle (payloadOut, sinkReadyOut)
