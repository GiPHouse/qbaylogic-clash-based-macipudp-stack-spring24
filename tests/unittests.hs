import Prelude

import Test.Tasty

import qualified Test.Cores.Ethernet.Ethernet

main :: IO ()
main = defaultMain $ testGroup "."
  [ Test.Cores.Ethernet.Ethernet.tests
  ]
