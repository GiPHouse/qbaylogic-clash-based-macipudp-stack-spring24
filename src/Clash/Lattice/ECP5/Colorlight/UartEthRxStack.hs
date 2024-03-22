module Clash.Lattice.ECP5.Colorlight.UartEthRxStack
( uartEthRxStack
) where

import Clash.Cores.Ethernet.PacketStream
import Clash.Cores.Ethernet.RGMII
import Clash.Lattice.ECP5.Colorlight.CRG
import Clash.Prelude
import Protocols

uartEthRxStack :: RGMIIRXChannel DomEth1 DomDDREth1 -> Signal dom bit
uartEthRxStack = undefined
