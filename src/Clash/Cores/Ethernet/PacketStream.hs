{-# language FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- Orphhan Hashable instances

module Clash.Cores.Ethernet.PacketStream
  ( PacketStreamM2S(..)
  , PacketStreamS2M(..)
  , PacketStream
  , unsafeToPacketStream
  , forceResetSanity
  ) where

import Clash.Prelude hiding ( sample )
import Prelude qualified as P

import Data.Hashable ( Hashable, hashWithSalt )
import Data.Maybe qualified as Maybe
import Data.Proxy

import Control.DeepSeq ( NFData )

import Protocols.Df qualified as Df
import Protocols.DfConv hiding ( pure )
import Protocols.Hedgehog.Internal
import Protocols.Internal

-- | Data sent from manager to subordinate, a simplified AXI4-Stream like interface
--   with metadata that can only change on packet delineation.
--   We bundled _tdest, _tuser and _tid into one big _meta field which holds metadata.
--   We don't have null or position bytes so _tstrb is replaced by a last indicator
--   that includes how many bytes are valid from the front of the vector.
--   _tvalid is modeled via wrapping this in a `Maybe`
data PacketStreamM2S (dataWidth :: Nat) (metaType :: Type)
  = PacketStreamM2S {
  _data :: Vec dataWidth (BitVector 8),
  -- ^ The bytes to be transmitted
  _last :: Maybe (Index dataWidth),
  -- ^ If Nothing, we are not yet at the last byte, otherwise signifies how many bytes of _data are valid
  _meta :: metaType,
  -- ^ the metaData of a packet, `_meta` must be constant during a packet.
  _abort :: Bool
  -- ^ If True, the current transfer is aborted and the slave should ignore the current transfer
} deriving (Generic, ShowX, Show, NFData, Bundle)

-- | Data sent from the subordinate to the manager
-- The only information transmitted is whether the slave is ready to receive data
newtype PacketStreamS2M = PacketStreamS2M {
  _ready :: Bool
  -- ^ Iff True, the slave is ready to receive data
} deriving (Generic, ShowX, Show, NFData, Bundle, Eq, NFDataX)

-- This data type is used for communication between components
data PacketStream (dom :: Domain) (dataWidth :: Nat) (metaType :: Type)

deriving instance
  ( KnownNat dataWidth, NFDataX metaType)
  => NFDataX (PacketStreamM2S dataWidth metaType)

-- |
-- >>> fragment0 = PacketStreamM2S @3 @() ((0x34):> (0x43):> (0x21):>Nil) (Just 1) () True
-- >>> fragment1 = PacketStreamM2S @3 @() ((0x34):> (0x43):> (0x24):>Nil) (Just 1) () True
-- >>> fragment0 == fragment1
-- True
-- >>> fragment2 = PacketStreamM2S @3 @() ((0x34):> (0x43):> (0x21):>Nil) (Just 1) () True
-- >>> fragment3 = PacketStreamM2S @3 @() ((0x35):> (0x43):> (0x24):>Nil) (Just 1) () True
-- >>> fragment2 == fragment3
-- False
-- >>> fragment4 = PacketStreamM2S @3 @() ((0x34):> (0x43):> (0x21):>Nil) (Just 0) () True
-- >>> fragment5 = PacketStreamM2S @3 @() ((0x34):> (0x23):> (0x24):>Nil) (Just 0) () True
-- >>> fragment4 == fragment5
-- True
-- >>> fragment6 = PacketStreamM2S @3 @() ((0x34):> (0x23):> (0x24):>Nil) (Just 0) () True
-- >>> fragment7 = PacketStreamM2S @3 @() ((0x34):> (0x23):> (0x24):>Nil) (Just 1) () True
-- >>> fragment6 == fragment7
-- False
-- >>> fragment8 = PacketStreamM2S @3 @() ((0x34):> (0x23):> (0x24):>Nil) (Nothing) () True
-- >>> fragment9 = PacketStreamM2S @3 @() ((0x34):> (0x23):> (0x24):>Nil) (Nothing) () True
-- >>> fragment8 == fragment9
-- True
instance ( KnownNat dataWidth, Eq metaType) => Eq (PacketStreamM2S dataWidth metaType) where
  st1 == st2 = metaEq && abortEq && lastEq && dataEq
    where
      metaEq = _meta st1 == _meta st2
      abortEq = _abort st1 == _abort st2
      lastEq = _last st1 == _last st2
      n = maybe 0 (\i -> 8 * (fromIntegral $ maxBound - i)) (_last st1)
      dataEq = (shiftR (pack $ _data st1) n) == (shiftR (pack $ _data st2) n)

-- Orphan hashable instances
deriving instance (KnownNat n) => Hashable (BitVector n)
deriving instance (KnownNat n) => Hashable (Index n)
instance (KnownNat n, Hashable a) => Hashable (Vec n a) where
  hashWithSalt s v = hashWithSalt s (toList v)

deriving instance
  (KnownNat dataWidth, Hashable metaType)
  => Hashable (PacketStreamM2S dataWidth metaType)

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
  ( KnownNat dataWidth
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

-- | Converts a CSignal into a PacketStream. This is unsafe, because it drops backpressure.
unsafeToPacketStream :: Circuit (CSignal dom (Maybe (PacketStreamM2S n a))) (PacketStream dom n a)
unsafeToPacketStream = Circuit (\(CSignal fwdInS, _) -> (CSignal $ pure (), fwdInS))

-- | Converts a packetStream into a CSignal.
fromPacketStream :: Circuit (PacketStream dom n a) (CSignal dom (Maybe (PacketStreamM2S n a)))
fromPacketStream = Circuit (\(inFwd, _) -> (pure (PacketStreamS2M True), CSignal inFwd))

-- | Ensures a circuit does not send out ready on reset
forceResetSanity :: forall dom n meta. HiddenClockResetEnable dom => Circuit (PacketStream dom n meta) (PacketStream dom n meta)
forceResetSanity
  = Circuit (\(fwd, bwd) -> unbundle . fmap f . bundle $ (rstLow, fwd, bwd))
 where
  f (True,  _,   _  ) = (PacketStreamS2M False, Nothing)
  f (False, fwd, bwd) = (bwd, fwd)
  rstLow = unsafeToHighPolarity hasReset
