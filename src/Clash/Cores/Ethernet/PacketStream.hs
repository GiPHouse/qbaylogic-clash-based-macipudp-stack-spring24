{-# LANGUAGE FlexibleContexts #-}

module Clash.Cores.Ethernet.PacketStream where

import Clash.Prelude
import Protocols (Protocol, Fwd, Bwd)
import Protocols.Internal
import Protocols.DfConv hiding (pure)
import qualified Data.Maybe as Maybe
import  Data.Proxy
import  Protocols.Hedgehog.Internal

-- Simplified AXI4-Stream (master to slave).
-- We bundled _tstrb, _tdest and _tuser into one big _tmeta field which holds metadata.
-- We removed _tid.
-- No _tvalid, we wrap the Stream into a Maybe where the Stream is Nothing when _tvalid is False.
data PacketStreamM2S (dataWidth :: Nat) (metaType :: Type)
  = PacketStreamM2S {
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
newtype PacketStreamS2M = PacketStreamS2M {
  _ready :: Bool
  -- ^ If True, the slave is ready to receive data
} deriving (Generic, Show, ShowX, Bundle)

data PacketStream (dom :: Domain) (dataWidth :: Nat) (metaType :: Type)

instance Protocol (PacketStream dom dataWidth metaType) where
  type Fwd (PacketStream dom dataWidth metaType) = Signal dom (Maybe (PacketStreamM2S dataWidth metaType))
  type Bwd (PacketStream dom dataWidth metaType) = Signal dom PacketStreamS2M

instance Backpressure (PacketStream dom dataWidth metaType) where
  boolsToBwd _ = fromList_lazy . fmap PacketStreamS2M

instance DfConv (PacketStream dom dataWidth metaType) where
  type Dom (PacketStream dom dataWidth metaType) = dom
  type FwdPayload (PacketStream dom dataWidth metaType) = PacketStreamM2S dataWidth metaType

  toDfCircuit proxy = toDfCircuitHelper proxy s0 blankOtp stateFn where
    s0 = ()
    blankOtp = Nothing
    stateFn ack _ optItem 
      = pure (optItem, Nothing, Maybe.isJust optItem && _ready ack)

  fromDfCircuit proxy =  fromDfCircuitHelper proxy s0 blankOtp stateFn where
    s0 = ()
    blankOtp = PacketStreamS2M { _ready = False }
    stateFn m2s ack _ 
      = pure (PacketStreamS2M {_ready = ack }, m2s, False)

instance (KnownDomain dom) =>
  Simulate (PacketStream dom dataWidth metaType) where
  type SimulateFwdType (PacketStream dom dataWidth metaType) = [Maybe (PacketStreamM2S dataWidth metaType)]
  type SimulateBwdType (PacketStream dom dataWidth metaType) = [PacketStreamS2M]
  type SimulateChannels (PacketStream dom dataWidth metaType) = 1

  
  simToSigFwd _ = fromList_lazy
  simToSigBwd _ = fromList_lazy
  sigToSimFwd _ = sample_lazy
  sigToSimBwd _ = sample_lazy

  stallC conf (head -> (stallAck, stalls))
    = withClockResetEnable clockGen resetGen enableGen
    $ stall Proxy Proxy conf stallAck stalls



