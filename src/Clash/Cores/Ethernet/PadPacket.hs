{-# language RecordWildCards #-}
module Clash.Cores.Ethernet.UpConverter
  ( padPacketC
  ) where

import Clash.Cores.Ethernet.PacketStream
import Clash.Prelude
import Data.Maybe
import Protocols

data PadPacketState
  = Filling { cycles :: Index 64}
  | Full
  | Padding { cycles :: Index 64}
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
      :: PadPacketState
      -> (Maybe (PacketStreamM2S dataWidth ()), PacketStreamS2M)
      -> ( PadPacketState
         , (PacketStreamS2M, Maybe (PacketStreamM2S dataWidth ()))
         )
    go st (fwdIn, PacketStreamS2M inReady)
      = (nextState st fwdIn inReady, (bwdOut st fwdIn inReady, fwdOut st fwdIn inReady))

    -- If we receive backpressure, we don't need to change the state
    nextState st _ False = st

    -- If we are filling, then the next state depends on _last
    -- If _last is Nothing and we fill the 64 bytes, then go to Full state, otherwise keep filling
    -- If _last is Just, then go to Padding state if not full, oterhwise go to s0
    nextState (Filling n) (Just PacketStreamM2S {..}) True = case _last of
      Nothing -> if n < maxBound - (natToNum @dataWidth) then Filling (n + (natToNum @dataWidth)) else Full
      Just i  -> if n < maxBound - resize i then Padding (n + resize i) else s0

    -- If we are filling and don't receive any input, we keep filling
    nextState (Filling n) Nothing _ = Filling n

    -- If we are full, then we stay in Full state until _last is Just
    nextState Full (Just PacketStreamM2S {..}) True = case _last of
      Nothing -> Full
      Just _ -> s0

    -- If we are full and don't receive any input, stay full
    nextState Full Nothing _ = Full

    -- While padding, it does not matter if we receive input
    nextState (Padding n) _ True = if n > (natToNum @dataWidth) then Padding (n - (natToNum @dataWidth)) else s0

    bwdOut (Padding _) _ _ = PacketStreamS2M {_ready = False}
    bwdOut _ fwdIn inReady = PacketStreamS2M {_ready = isNothing fwdIn && inReady}

    fwdOut = undefined

padPacketC
  :: forall (dataWidth :: Nat) (dom :: Domain).
  HiddenClockResetEnable dom
  => 1 <= dataWidth
  => KnownNat dataWidth
  => Circuit (PacketStream dom dataWidth ()) (PacketStream dom dataWidth ())
padPacketC = fromSignals padPacket
