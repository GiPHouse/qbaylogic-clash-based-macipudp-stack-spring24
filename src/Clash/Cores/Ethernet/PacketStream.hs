{-# LANGUAGE FlexibleContexts #-}

module Clash.Cores.Ethernet.PacketStream where

import           Data.Hashable (Hashable, hashWithSalt)
import Control.DeepSeq (NFData)
import Clash.Prelude hiding (sample)
import Protocols.Internal
import qualified Protocols.Df as Df
import qualified Protocols.DfConv as DfConv
import Protocols.DfConv hiding (pure)
import qualified Data.Maybe as Maybe
import  Data.Proxy
import  Protocols.Hedgehog.Internal 
import qualified Prelude as P

deriving instance
  ( KnownNat dataWidth, NFDataX metaType)
  => NFDataX (PacketStreamM2S dataWidth metaType)

deriving instance
  ( KnownNat dataWidth, Eq metaType)
  => Eq (PacketStreamM2S dataWidth metaType)

deriving instance
  ( KnownNat dataWidth, Hashable metaType)
  => Hashable (PacketStreamM2S dataWidth metaType)
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
} deriving (Generic, ShowX, Show, NFData, Bundle)
-- Slave to master: only a simple signal which tells the master whether
-- the slave is ready to receive data
newtype PacketStreamS2M = PacketStreamS2M {
  _ready :: Bool
  -- ^ If True, the slave is ready to receive data
} deriving (Generic, ShowX, Show, NFData, Bundle, Eq, NFDataX)

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
  sigToSimFwd _ s = sample_lazy s
  sigToSimBwd _ s = sample_lazy s

  stallC conf (head -> (stallAck, stalls))
    = withClockResetEnable clockGen resetGen enableGen
    $ stall Proxy Proxy conf stallAck stalls

instance (KnownDomain dom) => 
  Drivable (PacketStream dom dataWidth metaType) where
  type ExpectType (PacketStream dom dataWidth metaType) = 
    [PacketStreamM2S dataWidth metaType]

  toSimulateType Proxy = fmap Just
  fromSimulateType Proxy = Maybe.catMaybes

  driveC conf vals
    = withClockResetEnable clockGen resetGen enableGen
    $ drive Proxy conf vals
  sampleC conf ckt
    = withClockResetEnable clockGen resetGen enableGen
    $ sample Proxy conf ckt
   

instance
  ( 
    KnownNat dataWidth
  , NFDataX metaType
  , NFData metaType
  , ShowX metaType
  , Show metaType
  , Eq metaType
  , KnownDomain dom ) =>
  Test (PacketStream dom dataWidth metaType) where

  expectToLengths Proxy = pure . P.length
  expectN Proxy options nExpected sampled
    = expectN (Proxy @(Df.Df dom _)) options nExpected
    $ Df.maybeToData <$> sampled
