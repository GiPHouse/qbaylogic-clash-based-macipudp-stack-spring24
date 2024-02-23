{-# LANGUAGE FlexibleContexts #-}

module Clash.Cores.Ethernet.PacketStream where

import Clash.Prelude
import Protocols (Protocol, Fwd, Bwd)
import Protocols.DfConv hiding (pure)
import qualified Data.Maybe as Maybe

-- Simplified AXI4-Stream (master to slave).
-- We bundled _tstrb, _tdest and _tuser into one big _tmeta field which holds metadata.
-- We removed _tid.
-- No _tvalid, we wrap the Stream into a Maybe where the Stream is Nothing when _tvalid is False.
data StreamM2S (dataWidth :: Nat) (metaType :: Type)
  = StreamM2S {
  _data :: Vec dataWidth (Unsigned 8),
  _byte_enable :: Maybe (Vec dataWidth Bool),
  -- ^ If Nothing, the entire byte is enabled by default, otherwise signifies which bytes are enabled
  _meta :: metaType,
  -- ^ the type of the metaData if necessary
  _abort :: Bool
  -- ^ If True, the current transfer is aborted and the slave should ignore the current transfer
} deriving (Generic, Show, ShowX, Bundle)

-- Slave to master: only a simple signal which tells the master whether
-- the slave is ready to receive data
newtype StreamS2M = StreamS2M {
  _ready :: Bool
  -- ^ If True, the slave is ready to receive data
} deriving (Generic, Show, ShowX, Bundle)

data Stream (dom :: Domain) (dataWidth :: Nat) (metaType :: Type)

instance Protocol (Stream dom dataWidth metaType) where
  type Fwd (Stream dom dataWidth metaType) = Signal dom (Maybe (StreamM2S dataWidth metaType))
  type Bwd (Stream dom dataWidth metaType) = Signal dom StreamS2M

instance DfConv (Stream dom dataWidth metaType) where
  type Dom (Stream dom dataWidth metaType) = dom
  type FwdPayload (Stream dom dataWidth metaType) = StreamM2S dataWidth metaType

  toDfCircuit proxy = toDfCircuitHelper proxy s0 blankOtp stateFn where
    s0 = ()
    blankOtp = Nothing
    stateFn ack _ optItem 
      = pure (optItem, Nothing, Maybe.isJust optItem && _ready ack)

  fromDfCircuit proxy =  fromDfCircuitHelper proxy s0 blankOtp stateFn where
    s0 = ()
    blankOtp = StreamS2M { _ready = False }
    stateFn m2s ack _ 
      = pure (StreamS2M {_ready = ack }, m2s, False)
