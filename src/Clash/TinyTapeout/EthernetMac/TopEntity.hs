{-# language FlexibleContexts #-}
{-# language NoMonomorphismRestriction #-}
{-# language NumericUnderscores #-}

module Clash.TinyTapeout.EthernetMac.TopEntity ( topEntity ) where

import Clash.Annotations.TH
import Clash.Cores.Ethernet.MII (MIIRXChannel, MIITXChannel)
import Clash.Explicit.Prelude
import Clash.Prelude ( exposeClockResetEnable )
import Clash.TinyTapeout.EthernetMac.EthStack (stack)

import Protocols (toSignals)

createDomain vSystem
  { vName="RP2040"
  , vPeriod=40000
  , vActiveEdge=Rising
  , vResetKind=Synchronous
  , vInitBehavior=Unknown
  , vResetPolarity=ActiveLow
  }

createDomain vSystem
  { vName="DomEthTx"
  , vPeriod=40000
  , vActiveEdge=Rising
  , vResetKind=Synchronous
  , vInitBehavior=Unknown
  , vResetPolarity=ActiveLow
  }

createDomain vSystem
  { vName="DomEthRx"
  , vPeriod=40000
  , vActiveEdge=Rising
  , vResetKind=Synchronous
  , vInitBehavior=Unknown
  , vResetPolarity=ActiveLow
  }

topEntity
  :: "ethRxClk" ::: Clock DomEthRx
  -> "ethTxClk" ::: Clock DomEthTx
  -> "rstN" ::: Reset DomEthRx
  -> "" ::: MIIRXChannel DomEthRx
  -> "" ::: MIITXChannel DomEthRx
topEntity rxClk _txClk rxRst rxChannel = snd $ ckt (rxChannel, pure ())
  where
    ckt = toSignals $ exposeClockResetEnable stack rxClk rxRst enableGen

makeTopEntity 'topEntity
