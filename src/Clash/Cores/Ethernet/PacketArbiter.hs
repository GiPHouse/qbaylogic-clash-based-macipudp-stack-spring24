module Clash.Cores.Ethernet.PacketArbiter
  ( packetArbiterC
  , ArbiterMode(..)
  , sampleOut, sampleOut'
  ) where

import Clash.Prelude
import Clash.Cores.Ethernet.PacketStream
import Protocols
import Data.Maybe
import qualified Data.Bifunctor as B
import qualified Data.List

-- | Collect mode for `packetArbiterC`
data ArbiterMode
  = RoundRobin
  -- ^ Collect in a round-robin fashion. Fair and cheaper than `Parallel`.
  | Parallel
  -- ^ Check components in parallel. This mode has a higher throughput, but is
  -- biased towards the last source and also slightly more expensive.

-- | Collects packets from all sources, respecting packet boundaries.
packetArbiterC
  :: forall dom p n a
   . ( HiddenClockResetEnable dom
     , KnownNat p
     , 1 <= p
     )
  => ArbiterMode
  -- ^ See `ArbiterMode`
  -> Circuit (Vec p (PacketStream dom n a)) (PacketStream dom n a)
packetArbiterC mode = Circuit (B.first unbundle . mealyB go (maxBound, True) . B.first bundle)
  where
    go
      :: (Index p, Bool)
      -> (Vec p (Maybe (PacketStreamM2S n a)), PacketStreamS2M)
      -> ((Index p, Bool), (Vec p PacketStreamS2M, Maybe (PacketStreamM2S n a)))
    go (i, first) (fwds, bwd) = ((i'', continue), (bwds, fwd))
      where
        bwds = replace i bwd (repeat (PacketStreamS2M False))
        fwd = fwds !! i
        continue = case fwd of
          Nothing -> first
          Just (PacketStreamM2S _ (Just _) _ _) -> True
          _ -> False
        i' = case mode of
          RoundRobin -> satSucc SatWrap i -- next index
          Parallel -> fromMaybe maxBound $ fold @(p - 1) (<|>) (zipWith (<$) indicesI fwds) -- index of last sink with data
        i'' = if continue then i' else i

dataIn =
    [Nothing, Just (PacketStreamM2S (0xFF :> Nil) Nothing () False), Nothing                                               , Nothing                                               , Just (PacketStreamM2S (0xFF :> Nil) (Just 0) () False), Nothing                                                        , Nothing, Nothing] :>
    [Nothing, Nothing                                              , Just (PacketStreamM2S (0x00 :> Nil) (Just 0) () False), Just (PacketStreamM2S (0x00 :> Nil) (Just 0) () False), Just (PacketStreamM2S (0x00 :> Nil) (Just 0) () False), Just (PacketStreamM2S (0x00 :> Nil) (Just 0) () False)] :>
    Data.List.repeat Nothing :>
    Data.List.repeat Nothing :>
    Data.List.repeat Nothing :>
    Nil
sampleOut = exposeClockResetEnable
  (simulateC (packetArbiterC RoundRobin) def dataIn)
  systemClockGen systemResetGen enableGen

sampleOut' mode = exposeClockResetEnable (simulateC (packetArbiterC mode) def (Data.List.repeat (Just $ PacketStreamM2S (0xAA :> Nil) (Just 0) () False) :> Data.List.repeat (Just $ PacketStreamM2S (0x00 :> Nil) (Just 0) () False) :> Data.List.repeat Nothing :> Nil)) systemClockGen systemResetGen enableGen
