import Prelude

import Test.Tasty

import Test.Cores.Ethernet.AsyncFIFO qualified
import Test.Cores.Ethernet.Depacketizer qualified
import Test.Cores.Ethernet.DownConverter qualified
import Test.Cores.Ethernet.FcsInserter qualified
import Test.Cores.Ethernet.InternetChecksum qualified
import Test.Cores.Ethernet.InterpacketGapInserter qualified
import Test.Cores.Ethernet.MacDepacketizer qualified
import Test.Cores.Ethernet.MacPacketizer qualified
import Test.Cores.Ethernet.PacketArbiter qualified
import Test.Cores.Ethernet.PacketBuffer qualified
import Test.Cores.Ethernet.PacketDispatcher qualified
import Test.Cores.Ethernet.PacketStream qualified
import Test.Cores.Ethernet.PaddingInserter qualified
import Test.Cores.Ethernet.PreambleStripper qualified
import Test.Cores.Ethernet.UpConverter qualified
import Test.Lattice.ECP5.UART qualified

main :: IO ()
main = defaultMain $ testGroup "."
  [ Test.Cores.Ethernet.AsyncFIFO.tests
    , Test.Cores.Ethernet.PacketArbiter.tests
    , Test.Cores.Ethernet.PacketStream.tests
    , Test.Cores.Ethernet.PaddingInserter.tests
    , Test.Lattice.ECP5.UART.tests
    , Test.Cores.Ethernet.UpConverter.tests
    , Test.Cores.Ethernet.DownConverter.tests
    , Test.Cores.Ethernet.PacketBuffer.tests
    , Test.Cores.Ethernet.PacketDispatcher.tests
    , Test.Cores.Ethernet.PreambleStripper.tests
    , Test.Cores.Ethernet.InterpacketGapInserter.tests
    , Test.Cores.Ethernet.Depacketizer.tests
    , Test.Cores.Ethernet.MacDepacketizer.tests
    , Test.Cores.Ethernet.MacPacketizer.tests
    , Test.Cores.Ethernet.InternetChecksum.tests
    , Test.Cores.Ethernet.FcsInserter.tests
  ]
