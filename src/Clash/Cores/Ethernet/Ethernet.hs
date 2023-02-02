module Clash.Cores.Ethernet.Ethernet (plus) where

import Clash.Prelude

-- | Add two numbers. Example:
--
-- >>> plus 3 5
-- 8
plus :: Signed 8 -> Signed 8 -> Signed 8
plus a b = a + b
