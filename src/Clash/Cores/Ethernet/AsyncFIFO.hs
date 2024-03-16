module Clash.Cores.Ethernet.AsyncFIFO
  (asyncFifoC) where

import Clash.Explicit.Prelude ( asyncFIFOSynchronizer )
import Clash.Prelude hiding ( empty )

import Protocols.Internal ( Circuit, fromSignals )

import Clash.Cores.Ethernet.PacketStream


-- ^ Asynchronous FIFO circuit that can be used to safely cross clock domains.
asyncFifoC :: forall (depth     :: Nat)
                     (dataWidth :: Nat)
                     (wDom      :: Domain)
                     (rDom      :: Domain)
                     (metaType  :: Type) .
  ( KnownDomain wDom, KnownDomain rDom
  , KnownNat depth, 2 <= depth
  , KnownNat dataWidth, 1 <= dataWidth
  , NFDataX metaType)
  => SNat depth
  -> Clock wDom
  -> Reset wDom
  -> Enable wDom
  -> Clock rDom
  -> Reset rDom
  -> Enable rDom
  -> Circuit (PacketStream wDom dataWidth metaType) (PacketStream rDom dataWidth metaType)
asyncFifoC depth wClk wRst wEn rClk rRst rEn = fromSignals ckt where
  ckt (fwdIn, bwdIn) = (bwdOut, fwdOut) where
    (element, empty, full) = asyncFIFOSynchronizer depth wClk rClk wRst rRst wEn rEn readReq fwdIn
    fwdOut = mux empty (pure Nothing) (Just <$> element)
    -- ^ If the FIFO is empty, we output Nothing. Else, we output the oldest element.
    bwdOut = PacketStreamS2M . not <$> full
    -- ^ Assert backpressure when the FIFO is full.
    readReq = not <$> empty .&&. _ready <$> bwdIn
    -- ^ Next component is ready to read if the fifo is not empty and it does not assert backpressure.
