{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Prelude

import Test.Tasty

import qualified Test.Cores.Ethernet.AsyncFIFO 
import qualified Test.Cores.Ethernet.Depacketizer 
import qualified Test.Cores.Ethernet.DownConverter 
import qualified Test.Cores.Ethernet.InternetChecksum 
import qualified Test.Cores.Ethernet.InterpacketGapInserter 
import qualified Test.Cores.Ethernet.MacDepacketizer 
import qualified Test.Cores.Ethernet.PacketArbiter 
import qualified Test.Cores.Ethernet.PacketBuffer 
import qualified Test.Cores.Ethernet.PacketDispatcher 
import qualified Test.Cores.Ethernet.PacketStream 
import qualified Test.Cores.Ethernet.PaddingInserter 
import qualified Test.Lattice.ECP5.UART
import qualified Test.Cores.Ethernet.UpConverter
import qualified Test.Cores.Ethernet.FcsInserter
import Clash.Cores.Crc (deriveHardwareCrc)
import Clash.Cores.Crc.Catalog (Crc32_ethernet)
import Data.Proxy
import Clash.Prelude

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
    , Test.Cores.Ethernet.InterpacketGapInserter.tests
    , Test.Cores.Ethernet.Depacketizer.tests
    , Test.Cores.Ethernet.MacDepacketizer.tests
    , Test.Cores.Ethernet.InternetChecksum.tests
    , Test.Cores.Ethernet.FcsInserter.tests
  ]
