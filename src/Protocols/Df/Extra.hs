{-# language PartialTypeSignatures #-}

{-|
Module      : Protocols.Df.Extra
Description : Extra utility functions for working with df.
-}
module Protocols.Df.Extra
  ( filterS
  , mapS
  , partitionS
  ) where

import Clash.Prelude

import Protocols
import Protocols.Df qualified as Df

import Data.Bifunctor as B


-- | Like `filter`, but can reason over signals.
filterS :: forall dom a. Signal dom (a -> Bool) -> Circuit (Df dom a) (Df dom a)
filterS fS = Circuit (unbundle . liftA2 go fS . bundle)
 where
  go _ (Df.NoData, _) = (Ack True, Df.NoData)
  go f (Df.Data d, ack)
    | f d = (ack, Df.Data d)
    | otherwise = (Ack True, Df.NoData)

-- | Like 'map', but can reason over signals.
mapS :: Signal dom (a -> b) -> Circuit (Df dom a) (Df dom b)
mapS fS = Circuit (unbundle . liftA2 go fS . bundle)
 where
  go f (fwd, bwd) = (bwd, f <$> fwd)

-- | Like 'partition', but can reason over signals.
partitionS ::  forall dom a. Signal dom (a -> Bool) -> Circuit (Df dom a) (Df dom a, Df dom a)
partitionS fS = Circuit (B.second unbundle . unbundle . liftA2 go fS . bundle . B.second bundle)
 where
  go f (Df.Data a, (ackT, ackF))
    | f a = (ackT, (Df.Data a, Df.NoData))
    | otherwise = (ackF, (Df.NoData, Df.Data a))
  go _ _ = (Ack False, (Df.NoData, Df.NoData))
