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
  -> "" ::: MIITXChannel DomEthTx
topEntity rxClk txClk rawRst rxChannel = snd $ ckt (rxChannel, pure ())
  where
    rxRst = resetSynchronizer rxClk rawRst
    txRst = convertReset rxClk txClk rawRst
    ckt = toSignals $ exposeClockResetEnable (stack rxClk rxRst) txClk txRst enableGen

makeTopEntity 'topEntity
