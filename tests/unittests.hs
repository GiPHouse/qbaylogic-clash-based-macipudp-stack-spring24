import Prelude

import Test.Tasty

<<<<<<< HEAD
import qualified Test.Cores.Ethernet.PacketStream

main :: IO ()
main = defaultMain $ testGroup "."
  [ Test.Cores.Ethernet.PacketStream.tests
=======
import qualified Test.Cores.Ethernet.UpConverter

main :: IO ()
main = defaultMain $ testGroup "."
  [ Test.Cores.Ethernet.UpConverter.tests
>>>>>>> cb4394f (Add UpConverter test to unittests.hs)
  ]
