module Clash.Cores.Ethernet.RGMIIAdapter
  (unsafeRgmiiRxAdapter) where

import Data.Maybe

import Clash.Cores.Ethernet.PacketStream
import Clash.Prelude
import Protocols.Internal

newtype RgmiiRxAdapterState = RgmiiRxAdapterState { _last_byte :: Maybe (BitVector 8) }
  deriving (Generic, NFDataX)

data RgmiiOutputStream (dom :: Domain)

instance Protocol (RgmiiOutputStream dom) where
  type Fwd (RgmiiOutputStream dom) = Signal dom (Bool, Maybe (BitVector 8))
  type Bwd (RgmiiOutputStream dom) = Signal dom ()


stateFunc :: RgmiiRxAdapterState
             -> ((Bool, Maybe (BitVector 8)), PacketStreamS2M)
             -> (RgmiiRxAdapterState, ((), Maybe (PacketStreamM2S 1 ())))
stateFunc s ((rxErr, inp), PacketStreamS2M bwd) = (nextState, ((), out))
  where
    nextState = RgmiiRxAdapterState { _last_byte = inp }
    out = if not bwd then Nothing else case _last_byte s of -- drop packets when receiving backpressure
      Nothing -> Nothing
      Just x -> Just PacketStreamM2S {
        _data = x :> Nil,
        _last = if isJust inp then Just 1 else Nothing,
        _meta = (),
        _abort = rxErr
      }

unsafeRgmiiRxAdapter :: KnownDomain dom
  => HiddenClockResetEnable dom
  => Circuit (RgmiiOutputStream dom) (PacketStream dom 1 ())
unsafeRgmiiRxAdapter = fromSignals ckt
    where
        ckt = mealyB stateFunc s0 where
          s0 = RgmiiRxAdapterState { _last_byte = Nothing }
