{-|
Module      : Clash.Cores.Ethernet.PacketDispatcher
Description : Provides a packet dispatcher, for splitting packet streams
-}
module Clash.Cores.Ethernet.PacketDispatcher
  ( packetDispatcherC
  ) where

import Clash.Cores.Ethernet.PacketStream
import Clash.Prelude
import Data.Bifunctor
import Protocols

-- | Routes packets depending on their metadata, using given routing functions.
--
-- Data is sent to at most one element of the output vector, for which the
-- dispatch function evaluates to true on the metadata of the input. If none of
-- the functions evaluate to true, the input is dropped. If more than one of the
-- predicates are true, the first one is picked.
--
-- Sends out packets in the same clock cycle as they are received.
packetDispatcherC
  :: forall (dom :: Domain) (p :: Nat) (n :: Nat) (a :: Type)
   . ( HiddenClockResetEnable dom
     , KnownNat p
     )
  => Vec p (a -> Bool)
  -- ^ Dispatch function. If function at index i returns true for the metaData it
  -- dispatches the current packet to that sink.
  -> Circuit (PacketStream dom n a) (Vec p (PacketStream dom n a))
packetDispatcherC fs = Circuit (second unbundle . unbundle . fmap go . bundle . second bundle)
 where
  go (Just x, bwds) = case findIndex id $ zipWith ($) fs (pure $ _meta x) of
    Just i -> (bwds !! i, replace i (Just x) (repeat Nothing))
    _ -> (PacketStreamS2M True, repeat Nothing)
  go _ = (PacketStreamS2M False, repeat Nothing)
