{-# language RecordWildCards #-}
{-|
Module      : Clash.Cores.Ethernet.PaddingInserter
Description : Provides paddingInserterC for padding ethernet frames to the minimum of 64 bytes
-}
module Clash.Cores.Ethernet.PaddingInserter
  ( paddingInserterC
  ) where

import Clash.Cores.Ethernet.PacketStream
import Clash.Prelude
import Data.Maybe
import Protocols ( Circuit, fromSignals )

-- | State of the paddingInserter circuit.
-- Counts up to ceil(64/dataWidth) packets, which is the required
-- amount to fulfill the minimum ethernet frame size of 64.
data PaddingInserterState (dataWidth :: Nat)
  = Filling { count :: Index (Div (63 + dataWidth) dataWidth)}
  | Full
  | Padding { count :: Index (Div (63 + dataWidth) dataWidth)}
  deriving (Eq, Show, Generic, NFDataX)

paddingInserter
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
paddingInserter = mealyB go s0
  where
    s0 = Filling 0
    go
      :: PaddingInserterState dataWidth
      -> (Maybe (PacketStreamM2S dataWidth ()), PacketStreamS2M)
      -> (PaddingInserterState dataWidth, (PacketStreamS2M, Maybe (PacketStreamM2S dataWidth ())))
    go st (fwdIn, PacketStreamS2M inReady)
      = (st', (bwdOut, fwdOut))
      where
        st' = if isJust fwdOut && not inReady then st else case (st, fwdIn) of
          -- If we are filling and receive input, then the next state depends on _last
          -- If _last is Nothing and we fill the 64 bytes, then go to Full state, otherwise keep filling
          -- If _last is Just, then go to Padding state if not full, otherwise go to s0
          (Filling n, Just PacketStreamM2S {..}) -> case _last of
            Nothing -> if n < maxBound then Filling (n + 1) else Full
            Just _  -> if n < maxBound then Padding (n + 1) else s0
          -- If we are full and receive input, then we stay in Full state until _last is Just
          (Full, Just PacketStreamM2S {..}) -> case _last of
            Nothing -> st
            Just _  -> s0
          -- While padding, it does not matter if we receive input
          (Padding n, _) -> if n < maxBound then Padding (n + 1) else s0
          -- Remaning cases: state is Filling or Full and no input
          _ -> st

        readyOut = case st of
          Padding _ -> False
          _         -> isJust fwdOut && inReady

        bwdOut = PacketStreamS2M {_ready = readyOut}

        -- Calculate the index of _last needed after padding
        calcMod = mod (63 :: Index (Max 64 dataWidth)) (natToNum @dataWidth)

        lastOut = case st of
          Filling n -> if n == maxBound && st' == s0
            -- If _last is bigger than calcMod, then don't add padding
            -- Otherwise, pad to calcMod
            then Just (max (fromMaybe 0 (_last (fromJust fwdIn))) (resize calcMod))
            else Nothing
          Full      -> _last =<< fwdIn
          Padding _ -> if st' == s0 then Just (resize calcMod) else Nothing

        fwdOut = case st of
          Padding _ -> Just PacketStreamM2S{_data = repeat 0, _last = lastOut, _meta = (), _abort = False}
          _         -> if isJust fwdIn then Just (fromJust fwdIn){_last = lastOut} else fwdIn

-- | Pads ethernet frames to the minimum of 64 bytes
paddingInserterC
  :: forall (dataWidth :: Nat) (dom :: Domain).
  HiddenClockResetEnable dom
  => 1 <= dataWidth
  => KnownNat dataWidth
  => Circuit (PacketStream dom dataWidth ()) (PacketStream dom dataWidth ())
paddingInserterC = fromSignals paddingInserter
