module Clash.Cores.Ethernet.RGMIIAdapter
  (unsafeRGMIIRxAdapter, RGMIIOutputStream) where

import Data.Maybe

import Clash.Cores.Ethernet.PacketStream
import Clash.Prelude
import Protocols.Internal

newtype RGMIIRxAdapterState = RGMIIRxAdapterState { _last_byte :: Maybe (BitVector 8) }
  deriving (Generic, NFDataX)

data RGMIIOutputStream (dom :: Domain)

instance Protocol (RGMIIOutputStream dom) where
  type Fwd (RGMIIOutputStream dom) = Signal dom (Bool, Maybe (BitVector 8))
  type Bwd (RGMIIOutputStream dom) = Signal dom ()


stateFunc :: RGMIIRxAdapterState
             -> ((Bool, Maybe (BitVector 8)), PacketStreamS2M)
             -> (RGMIIRxAdapterState, ((), Maybe (PacketStreamM2S 1 ())))
stateFunc s ((rxErr, inp), PacketStreamS2M bwd) = (nextState, ((), out))
  where
    nextState = RGMIIRxAdapterState { _last_byte = inp }
    out = if not bwd then Nothing else case _last_byte s of -- drop packets when receiving backpressure
      Nothing -> Nothing
      Just x -> Just PacketStreamM2S {
        _data = x :> Nil,
        _last = if isJust inp then Just 1 else Nothing,
        _meta = (),
        _abort = rxErr
      }

unsafeRGMIIRxAdapter :: KnownDomain dom
  => HiddenClockResetEnable dom
  => Circuit (RGMIIOutputStream dom) (PacketStream dom 1 ())
unsafeRGMIIRxAdapter = fromSignals ckt
    where
        ckt = mealyB stateFunc s0 where
          s0 = RGMIIRxAdapterState { _last_byte = Nothing }
