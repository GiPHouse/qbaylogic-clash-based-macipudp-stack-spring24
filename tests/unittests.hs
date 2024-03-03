import Prelude

import Test.Tasty

import qualified Test.Cores.Ethernet.PacketStream

main :: IO ()
main = defaultMain $ testGroup "."
  [ Test.Cores.Ethernet.PacketStream.tests
  ]
