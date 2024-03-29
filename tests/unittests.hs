import Prelude

import Test.Tasty

import qualified Test.Cores.Ethernet.PacketStream
import qualified Test.Lattice.ECP5.UART
import qualified Test.Cores.Ethernet.UpConverter
import qualified Test.Cores.Ethernet.DownConverter
import qualified Test.Cores.Ethernet.PacketBuffer
import qualified Test.Cores.Ethernet.MaybeControl
import qualified Test.Cores.Ethernet.MaybeControlProperty


main :: IO ()
main = defaultMain $ testGroup "."
  [ 
    Test.Cores.Ethernet.PacketStream.tests
    , Test.Lattice.ECP5.UART.tests
    , Test.Cores.Ethernet.UpConverter.tests
    , Test.Cores.Ethernet.DownConverter.tests
    , Test.Cores.Ethernet.MaybeControlProperty.tests
    , Test.Cores.Ethernet.PacketBuffer.tests
  ]
