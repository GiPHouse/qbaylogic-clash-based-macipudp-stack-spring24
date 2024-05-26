{-|
Module      : Clash.Cores.Ethernet.Examples.RxStack
Description : Provides the entire receive stack as a circuit.
-}
{-# language FlexibleContexts #-}
module Clash.Cores.Ethernet.Examples.RxStack
( rxStack
) where

import Clash.Cores.Crc
import Clash.Cores.Crc.Catalog
import Clash.Cores.Ethernet.Mac.EthernetTypes ( EthernetHeader, MacAddress )
import Clash.Cores.Ethernet.Mac.FrameCheckSequence ( fcsValidatorC )
import Clash.Cores.Ethernet.Mac.MacPacketizers ( macDepacketizerC )
import Clash.Cores.Ethernet.Mac.Preamble ( preambleStripperC )
import Clash.Prelude
import Protocols
import Protocols.Extra.PacketStream
import Protocols.Extra.PacketStream.AsyncFIFO ( asyncFifoC )
import Protocols.Extra.PacketStream.UpConverter ( upConverterC )

-- | Processes received ethernet frames
rxStack
  :: forall
  (dataWidth :: Nat)
  (dom :: Domain)
  (domEth :: Domain).
  ( HiddenClockResetEnable dom
  , KnownDomain domEth
  , HardwareCrc Crc32_ethernet 8 dataWidth
  )
  => 1 <= dataWidth
  => KnownNat dataWidth
  => Clock domEth
  -> Reset domEth
  -> Enable domEth
  -> Signal dom MacAddress
  -> Circuit (PacketStream domEth 1 ()) (PacketStream dom dataWidth EthernetHeader)
rxStack ethClk ethRst ethEn _macAddressS = stack
  where
    upConverterC' = exposeClockResetEnable upConverterC ethClk ethRst ethEn
    stack = upConverterC'
             |> asyncFifoC d4 ethClk ethRst ethEn hasClock hasReset hasEnable
             |> preambleStripperC
             |> fcsValidatorC
             |> macDepacketizerC
             -- TODO: filter on macAddressS or broadcoast
