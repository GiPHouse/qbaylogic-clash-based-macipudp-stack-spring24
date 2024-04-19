{-# language RecordWildCards #-}
module Clash.Cores.Ethernet.PadPacket
  ( padPacketC
  ) where

import Clash.Cores.Ethernet.PacketStream
import Clash.Prelude
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

        fwdOut = case st of
          Padding _ -> Just PacketStreamM2S{_data = repeat 0, _last = lastOut, _meta = (), _abort = False}
          _         -> if isJust fwdIn then Just (fromJust fwdIn){_last = lastOut} else fwdIn

padPacketC
  :: forall (dataWidth :: Nat) (dom :: Domain).
  HiddenClockResetEnable dom
  => 1 <= dataWidth
  => KnownNat dataWidth
  => Circuit (PacketStream dom dataWidth ()) (PacketStream dom dataWidth ())
padPacketC = fromSignals padPacket
