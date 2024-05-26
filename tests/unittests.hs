import Prelude

import Test.Tasty

import Test.Cores.Arp.ArpManager qualified
import Test.Cores.Arp.ArpReceiver qualified
import Test.Cores.Arp.ArpTable qualified
import Test.Cores.Arp.ArpTransmitter qualified
import Test.Cores.Ethernet.AsyncFIFO qualified
import Test.Cores.Ethernet.DownConverter qualified
import Test.Cores.Ethernet.EthernetTypes qualified
import Test.Cores.Ethernet.FrameCheckSequence qualified
import Test.Cores.Ethernet.InternetChecksum qualified
import Test.Cores.Ethernet.InterpacketGapInserter qualified
import Test.Cores.Ethernet.IpPacketizer qualified
import Test.Cores.Ethernet.MacDepacketizer qualified
import Test.Cores.Ethernet.MacPacketizer qualified
import Test.Cores.Ethernet.PacketArbiter qualified
import Test.Cores.Ethernet.PacketBuffer qualified
import Test.Cores.Ethernet.PacketDispatcher qualified
import Test.Cores.Ethernet.PacketStream qualified
import Test.Cores.Ethernet.PaddingInserter qualified
import Test.Cores.Ethernet.PreambleInserter qualified
import Test.Cores.Ethernet.PreambleStripper qualified
import Test.Cores.Ethernet.UpConverter qualified
import Test.Cores.IP.Icmp qualified
import Test.Cores.IP.IPDepacketizer qualified

main :: IO ()
main = defaultMain $ testGroup "."
  [ Test.Cores.Arp.ArpManager.tests
    , Test.Cores.Arp.ArpReceiver.tests
    , Test.Cores.Arp.ArpTable.tests
    , Test.Cores.Arp.ArpTransmitter.tests
    , Test.Cores.Ethernet.AsyncFIFO.tests
    , Test.Cores.Ethernet.PacketArbiter.tests
    , Test.Cores.Ethernet.PacketStream.tests
    , Test.Cores.Ethernet.PaddingInserter.tests
    , Test.Cores.Ethernet.UpConverter.tests
    , Test.Cores.Ethernet.DownConverter.tests
    , Test.Cores.Ethernet.PacketBuffer.tests
    , Test.Cores.Ethernet.PacketDispatcher.tests
    , Test.Cores.Ethernet.PreambleInserter.tests
    , Test.Cores.Ethernet.PreambleStripper.tests
    , Test.Cores.Ethernet.InterpacketGapInserter.tests
    , Test.Cores.Ethernet.MacDepacketizer.tests
    , Test.Cores.Ethernet.MacPacketizer.tests
    , Test.Cores.Ethernet.InternetChecksum.tests
    , Test.Cores.Ethernet.FrameCheckSequence.testsValidate
    , Test.Cores.Ethernet.FrameCheckSequence.testsInsert
    , Test.Cores.Ethernet.EthernetTypes.tests
    , Test.Cores.IP.Icmp.tests
    , Test.Cores.IP.IPDepacketizer.tests
    , Test.Cores.Ethernet.IpPacketizer.tests
  ]
