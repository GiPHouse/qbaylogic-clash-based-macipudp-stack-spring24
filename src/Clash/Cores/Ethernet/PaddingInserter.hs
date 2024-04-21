{-|
Module      : Clash.Cores.Ethernet.PaddingInserter
Description : Provides paddingInserterC for padding ethernet frames to the minimum of 64 bytes
-}
module Clash.Cores.Ethernet.PaddingInserter
  ( paddingInserterC
  ) where

import Clash.Cores.Ethernet.PacketStream
import Clash.Cores.Ethernet.Util ( toMaybe )
import Clash.Prelude
import Control.Monad ( guard )
import Data.Maybe ( isJust )
import Protocols ( Circuit, fromSignals )


-- | State of the paddingInserter circuit.
-- Counts up to ceil(64/dataWidth) packets, which is the required
-- amount to fulfill the minimum ethernet frame size of 64.
data PaddingInserterState (dataWidth :: Nat)
  = Filling { count :: Index (DivRU 64 dataWidth)}
  | Full
  | Padding { count :: Index (DivRU 64 dataWidth)}
  deriving (Eq, Show, Generic, NFDataX)

paddingInserter
  :: forall (dataWidth :: Nat) (dom :: Domain) .
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
paddingInserter = mealyB go (Filling 0)
  where
    padding = PacketStreamM2S {_data = repeat 0, _last = Nothing, _meta = (), _abort = False}
    lastIdx = natToNum @(63 `Mod` dataWidth)
    go
      :: PaddingInserterState dataWidth
      -> (Maybe (PacketStreamM2S dataWidth ()), PacketStreamS2M)
      -> (PaddingInserterState dataWidth, (PacketStreamS2M, Maybe (PacketStreamM2S dataWidth ())))
    -- If state is Full, forward the input from sink
    go Full (Nothing, bwd) = (Full, (bwd, Nothing))
    go Full (Just fwd, PacketStreamS2M inReady) = (if inReady && isJust (_last fwd) then Filling 0 else Full, (PacketStreamS2M inReady, Just fwd))

    -- If state is Padding, send out zero-bytes to source and backpressure to sink
    go st@(Padding i) (_, PacketStreamS2M inReady) = (if inReady then st' else st, (PacketStreamS2M False, Just fwdOut))
      where
        st' = if i == maxBound then Filling 0 else Padding (i + 1)
        fwdOut = padding {_last = toMaybe (i == maxBound) lastIdx}

    -- If state is Filling, forward the input from sink with updated _last
    go (Filling i) (Nothing, bwd) = (Filling i, (bwd, Nothing))
    go st@(Filling i) (Just fwdIn, PacketStreamS2M inReady) = (if inReady then st' else st, (PacketStreamS2M inReady, Just fwdOut))
      where
        st' = case (i == maxBound, _last fwdIn) of
          (True, Nothing) -> Full
          (True, Just _) -> Filling 0
          (False, Nothing) -> Filling (i + 1)
          (False, Just _) -> Padding (i + 1)
        -- If i < maxBound, then set _last to Nothing
        -- Otherwise, set _last to the maximum of the
        -- index that would reach the minimum frame size,
        -- and the _last of fwdIn
        fwdOut = fwdIn {_last = guard (i == maxBound) >> max lastIdx <$> _last fwdIn}

-- | Pads ethernet frames to the minimum of 64 bytes.
-- Assumes that all invalid bytes are set to 0.
paddingInserterC
  :: forall (dataWidth :: Nat) (dom :: Domain).
  HiddenClockResetEnable dom
  => 1 <= dataWidth
  => KnownNat dataWidth
  => Circuit (PacketStream dom dataWidth ()) (PacketStream dom dataWidth ())
paddingInserterC = fromSignals paddingInserter
