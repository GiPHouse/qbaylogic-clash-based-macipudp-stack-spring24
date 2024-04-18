{-# language RecordWildCards #-}
module Clash.Cores.Ethernet.PadPacket
  ( padPacketC
  ) where

import Clash.Cores.Ethernet.PacketStream
import Clash.Prelude
import Data.Maybe
import Protocols

data PadPacketState (dataWidth :: Nat)
  = Filling { cycles :: Index dataWidth}
  | Full
  | Padding { cycles :: Index dataWidth}
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
      -> ( PadPacketState dataWidth
         , (PacketStreamS2M, Maybe (PacketStreamM2S dataWidth ()))
         )
    go st (fwdIn, PacketStreamS2M inReady)
      = (st', (bwdOut, fwdOut))
      where
        st' = if not inReady then st else case (st, fwdIn) of
          -- If we are filling, then the next state depends on _last
          -- If _last is Nothing and we fill the 64 bytes, then go to Full state, otherwise keep filling
          -- If _last is Just, then go to Padding state if not full, otherwise go to s0
          (Filling n, Just PacketStreamM2S {..}) -> case _last of
            Nothing -> if n < maxBound - (natToNum @dataWidth) then Filling (n + (natToNum @dataWidth)) else Full
            Just i  -> if n < maxBound - resize i then Padding (n + resize i) else s0
          -- If we are filling and don't receive any input, we keep filling
          (Filling _, Nothing) -> st
          -- If we are full, then we stay in Full state until _last is Just
          (Full, Just PacketStreamM2S {..}) -> case _last of
            Nothing -> st
            Just _ -> s0
          -- If we are full and don't receive any input, stay full
          (Full, Nothing) -> st
          -- While padding, it does not matter if we receive input
          (Padding n, _) -> if n > (natToNum @dataWidth) then Padding (n - (natToNum @dataWidth)) else s0

        bwdOut = case st of
          Padding _ -> PacketStreamS2M {_ready = False}
          _         -> PacketStreamS2M {_ready = isNothing fwdIn && inReady}

        lastOut = case (st, fwdIn) of
          (Filling _, Just PacketStreamM2S {..}) -> if st' == s0 then _last else Nothing
          (Full, Just PacketStreamM2S {..}) -> if st' == s0 then _last else Nothing
          (Padding n, _) -> if st' == s0 then Just n else Nothing
          _ -> Nothing

        fwdOut = case (st, fwdIn) of
          (Filling _, Just PacketStreamM2S {..}) -> Just PacketStreamM2S{_data = _data, _last = lastOut, _meta = _meta, _abort = _abort}
          (Full, Just PacketStreamM2S {..}) -> Just PacketStreamM2S{_data = _data, _last = lastOut, _meta = _meta, _abort = _abort}
          (Padding _, _) -> Just PacketStreamM2S{_data = repeat 0, _last = lastOut, _meta = (), _abort = False}
          _ -> Nothing

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
