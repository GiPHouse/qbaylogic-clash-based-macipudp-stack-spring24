module Clash.TinyTapeout.EthernetMac.Credits where

import Clash.Prelude
import Clash.Sized.Vector (unsafeFromList)

import Data.Char (ord)
import qualified Data.List as L

creditsStr :: String
creditsStr = L.concat
  [ "Designed by:"
  , "\n\tJasper Laumen"
  , "\n\tMart Koster"
  , "\n\tDaan Weessies"
  , "\n\tCato van Ojen"
  , "\n\tJasmijn Bookelmann"
  , "\n\tTim Wallet"
  , "\n\tBryan Rinders"
  , "\n\tMatthijs Muis"
  , "\n\tRowan Goemans"
  , "\n\tPeter Lebbing"
  , "\n\n ==> Sponsored by QbayLogic <=="
  ]

chunkList :: Int -> [a] -> [[a]]
chunkList _ [] = []
chunkList n xs = as : chunkList n bs where (as,bs) = L.splitAt n xs

alignTo :: Int -> String -> String
alignTo n str = str L.++ L.replicate (n - mod (L.length str) n) '\0'

-- | Get the credit string in packed big endian format
creditsBV :: SNat n -> [BitVector (n * 8)]
creditsBV n@SNat = fmap (pack . unsafeFromList)
                     $ chunkList (snatToNum n)
                     $ fmap (fromIntegral @Int @(BitVector 8) . ord)
                     $ alignTo (snatToNum n) creditsStr
