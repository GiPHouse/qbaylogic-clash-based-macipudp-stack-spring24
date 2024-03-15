import Prelude

import Test.Tasty

import qualified Test.Cores.Ethernet.PacketStream
import qualified Test.Cores.Ethernet.UpConverter

main :: IO ()
main = defaultMain $ testGroup "."
  [ Test.Cores.Ethernet.PacketStream.tests
  , Test.Cores.Ethernet.UpConverter.tests
  ]
