module Clash.Cores.Ethernet.Util
    ( toMaybe
    , extractSignal
    ) where

import Clash.Prelude
import Protocols.Internal (CSignal(CSignal))

-- | Wrap a value in a Just if True
toMaybe :: Bool -> a -> Maybe a
toMaybe True x = Just x
toMaybe False _ = Nothing

-- | Extract Signal from CSignal
extractSignal :: CSignal dom a -> Signal dom a
extractSignal (CSignal x) = x
