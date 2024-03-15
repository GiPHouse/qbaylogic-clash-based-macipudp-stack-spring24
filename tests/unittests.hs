import Prelude

import Test.Tasty

import qualified Test.Cores.Ethernet.PacketStream
import qualified Test.Lattice.ECP5.UART

main :: IO ()
main = defaultMain $ testGroup "."
  [ Test.Cores.Ethernet.PacketStream.tests
  , Test.Lattice.ECP5.UART.tests
  ]
