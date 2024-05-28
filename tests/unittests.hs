import Prelude

import Test.Tasty

import Test.Cores.Ethernet.Arp.ArpManager qualified
import Test.Cores.Ethernet.Arp.ArpTable qualified
import Test.Cores.Ethernet.Icmp qualified
import Test.Cores.Ethernet.IP.EthernetStream qualified
import Test.Cores.Ethernet.IP.InternetChecksum qualified
import Test.Cores.Ethernet.IP.IPPacketizers qualified
import Test.Cores.Ethernet.Mac.EthernetTypes qualified
import Test.Cores.Ethernet.Mac.FrameCheckSequence qualified
import Test.Cores.Ethernet.Mac.InterpacketGapInserter qualified
import Test.Cores.Ethernet.Mac.MacPacketizers qualified
import Test.Cores.Ethernet.Mac.PaddingInserter qualified
import Test.Cores.Ethernet.Mac.Preamble qualified
import Test.Protocols.Extra.PacketStream qualified
import Test.Protocols.Extra.PacketStream.AsyncFIFO qualified
import Test.Protocols.Extra.PacketStream.Converters qualified
import Test.Protocols.Extra.PacketStream.PacketBuffer qualified
import Test.Protocols.Extra.PacketStream.Packetizers qualified
import Test.Protocols.Extra.PacketStream.Routing qualified


main :: IO ()
main = defaultMain $ testGroup "."
  [ Test.Cores.Ethernet.Arp.ArpManager.tests
  , Test.Cores.Ethernet.Arp.ArpTable.tests
  , Test.Cores.Ethernet.IP.EthernetStream.tests
  , Test.Cores.Ethernet.IP.InternetChecksum.tests
  , Test.Cores.Ethernet.IP.IPPacketizers.tests
  , Test.Cores.Ethernet.Mac.EthernetTypes.tests
  , Test.Cores.Ethernet.Mac.FrameCheckSequence.tests
  , Test.Cores.Ethernet.Mac.InterpacketGapInserter.tests
  , Test.Cores.Ethernet.Mac.MacPacketizers.tests
  , Test.Cores.Ethernet.Mac.PaddingInserter.tests
  , Test.Cores.Ethernet.Mac.Preamble.tests
  , Test.Cores.Ethernet.Icmp.tests
  , Test.Protocols.Extra.PacketStream.AsyncFIFO.tests
  , Test.Protocols.Extra.PacketStream.Converters.tests
  , Test.Protocols.Extra.PacketStream.PacketBuffer.tests
  , Test.Protocols.Extra.PacketStream.Packetizers.tests
  , Test.Protocols.Extra.PacketStream.Routing.tests
  , Test.Protocols.Extra.PacketStream.tests
  ]
