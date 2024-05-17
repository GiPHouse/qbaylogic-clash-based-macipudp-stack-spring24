{-|
Module      : Clash.Cores.Ethernet.PacketArbiter
Description : Provides a packet arbiter, for merging packet streams
-}
module Clash.Cores.Ethernet.PacketArbiter
  ( packetArbiterC
  , ArbiterMode (..)
  ) where

import Clash.Cores.Ethernet.PacketStream
import Clash.Prelude
import Data.Bifunctor qualified as B
import Data.Maybe
import Protocols

-- | Collect mode for `packetArbiterC`
data ArbiterMode
  = -- | Collect in a round-robin fashion. Fair and cheaper than `Parallel`.
    RoundRobin
  | -- | Check components in parallel. This mode has a higher throughput, but is
    -- biased towards the last source and also slightly more expensive.
    Parallel

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
  go (i, first) (fwds, bwd@(PacketStreamS2M ack)) = ((i', continue), (bwds, fwd))
   where
    bwds = replace i bwd (repeat (PacketStreamS2M False))
    fwd = fwds !! i
    continue = case fwd of
      Nothing -> first -- only switch sources if this is not somewhere inside a packet
      Just (PacketStreamM2S _ (Just _) _ _) -> ack -- switch source once last packet is acknowledged
      _ -> False
    i' = case (mode, continue) of
      (_, False) -> i
      (RoundRobin, _) -> satSucc SatWrap i -- next index
      (Parallel, _) -> fromMaybe maxBound $ fold @(p - 1) (<|>) (zipWith (<$) indicesI fwds) -- index of last sink with data
