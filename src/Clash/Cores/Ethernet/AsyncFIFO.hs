{-|
Module      : Clash.Cores.Ethernet.AsyncFIFO
Description : Provides `asyncFifoC` for crossing clock domains in the packet stream protocol
-}
module Clash.Cores.Ethernet.AsyncFIFO (asyncFifoC) where

import Clash.Explicit.Prelude (asyncFIFOSynchronizer)
import Clash.Prelude hiding (empty)

import Protocols.Internal (Circuit, fromSignals)

import Clash.Cores.Ethernet.PacketStream

-- | Asynchronous FIFO circuit that can be used to safely cross clock domains.
-- Uses `Clash.Explicit.Prelude.asyncFIFOSynchronizer` internally.
asyncFifoC
  :: forall
    (depth :: Nat)
    (dataWidth :: Nat)
    (wDom :: Domain)
    (rDom :: Domain)
    (metaType :: Type)
   . ( KnownDomain wDom
     , KnownDomain rDom
     , KnownNat depth
     , 2 <= depth
     , KnownNat dataWidth
     , 1 <= dataWidth
     , NFDataX metaType
     )
  => SNat depth
  -- ^ 2^depth is the number of elements this component can store
  -> Clock wDom
  -- ^ Clock signal in the write domain
  -> Reset wDom
  -- ^ Reset signal in the write domain
  -> Enable wDom
  -- ^ Enable signal in the write domain
  -> Clock rDom
  -- ^ Clock signal in the read domain
  -> Reset rDom
  -- ^ Reset signal in the read domain
  -> Enable rDom
  -- ^ Enable signal in the read domain
  -> Circuit (PacketStream wDom dataWidth metaType) (PacketStream rDom dataWidth metaType)
asyncFifoC depth wClk wRst wEn rClk rRst rEn = fromSignals ckt
 where
  ckt (fwdIn, bwdIn) = (bwdOut, fwdOut)
   where
    (element, empty, full) = asyncFIFOSynchronizer depth wClk rClk wRst rRst wEn rEn readReq fwdIn
    -- If the FIFO is empty, we output Nothing. Else, we output the oldest element.
    fwdOut = mux empty (pure Nothing) (Just <$> element)
    -- Assert backpressure when the FIFO is full.
    bwdOut = PacketStreamS2M . not <$> full
    -- Next component is ready to read if the fifo is not empty and it does not assert backpressure.
    readReq = (not <$> empty) .&&. (_ready <$> bwdIn)
