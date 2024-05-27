import Prelude

import Test.Tasty

import Test.Cores.Ethernet.Arp qualified
import Test.Cores.Ethernet.Arp.ArpManager qualified
import Test.Cores.Ethernet.Arp.ArpTable qualified
import Test.Cores.Ethernet.Icmp qualified
import Test.Cores.Ethernet.IP.InternetChecksum qualified
import Test.Cores.Ethernet.IP.IPPacketizers qualified
import Test.Cores.Ethernet.Mac.AsyncFIFO qualified
import Test.Cores.Ethernet.Mac.EthernetTypes qualified
import Test.Cores.Ethernet.Mac.FrameCheckSequence qualified
import Test.Cores.Ethernet.Mac.InterpacketGapInserter qualified
import Test.Cores.Ethernet.Mac.MacDepacketizer qualified
import Test.Cores.Ethernet.Mac.MacPacketizer qualified
import Test.Cores.Ethernet.Mac.PaddingInserter qualified
import Test.Cores.Ethernet.Mac.PreambleInserter qualified
import Test.Cores.Ethernet.Mac.PreambleStripper qualified
import Test.Protocols.Extra.PacketStream qualified
import Test.Protocols.Extra.PacketStream.DownConverter qualified
import Test.Protocols.Extra.PacketStream.PacketArbiter qualified
import Test.Protocols.Extra.PacketStream.PacketBuffer qualified
import Test.Protocols.Extra.PacketStream.PacketDispatcher qualified
import Test.Protocols.Extra.PacketStream.UpConverter qualified

main :: IO ()
main = defaultMain $ testGroup "."
  [ Test.Cores.Ethernet.Arp.ArpManager.tests
  , Test.Cores.Ethernet.Arp.testsReceiver
  , Test.Cores.Ethernet.Arp.ArpTable.tests
  , Test.Cores.Ethernet.Arp.testsTransmitter
  , Test.Cores.Ethernet.Mac.AsyncFIFO.tests
  , Test.Protocols.Extra.PacketStream.PacketArbiter.tests
  , Test.Protocols.Extra.PacketStream.tests
  , Test.Cores.Ethernet.Mac.PaddingInserter.tests
  , Test.Protocols.Extra.PacketStream.UpConverter.tests
  , Test.Protocols.Extra.PacketStream.DownConverter.tests
  , Test.Protocols.Extra.PacketStream.PacketBuffer.tests
  , Test.Protocols.Extra.PacketStream.PacketDispatcher.tests
  , Test.Cores.Ethernet.Mac.PreambleInserter.tests
  , Test.Cores.Ethernet.Mac.PreambleStripper.tests
  , Test.Cores.Ethernet.Mac.InterpacketGapInserter.tests
  , Test.Cores.Ethernet.Mac.MacDepacketizer.tests
  , Test.Cores.Ethernet.Mac.MacPacketizer.tests
  , Test.Cores.Ethernet.IP.InternetChecksum.tests
  , Test.Cores.Ethernet.Mac.FrameCheckSequence.testsValidate
  , Test.Cores.Ethernet.Mac.FrameCheckSequence.testsInsert
  , Test.Cores.Ethernet.Mac.EthernetTypes.tests
  , Test.Cores.Ethernet.Icmp.tests
  , Test.Cores.Ethernet.IP.IPPacketizers.testsDepacketizer
  , Test.Cores.Ethernet.IP.IPPacketizers.testsPacketizer
  ]
