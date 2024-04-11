-- | Utility module for small, low-complexity functions. Please keep this small.
module Clash.Cores.Ethernet.Util
    ( toMaybe
    ) where

import Clash.Prelude

-- | Wrap a value in a Just if True
toMaybe :: Bool -> a -> Maybe a
toMaybe True x = Just x
toMaybe False _ = Nothing
