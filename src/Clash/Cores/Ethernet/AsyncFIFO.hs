{-|
Module      : Clash.Cores.Ethernet.AsyncFIFO
Description : Provides `asyncFifoC` for crossing clock domains in the packet stream protocol
-}
module Clash.Cores.Ethernet.AsyncFIFO
  (asyncFifoC) where

import Data.Constraint             ((:-)(..), Dict (..))
import Data.Constraint.Nat         (leTrans)
import Data.Maybe                  (isJust)

import qualified Clash.Explicit.Prelude as E
import Clash.Prelude hiding ( empty )

import Protocols.Internal ( Circuit, fromSignals )

import Clash.Cores.Ethernet.PacketStream

-- Copied asyncFIFO implementation due to variable naming of CDC register variables
mealyRegNamed
  :: forall (name :: Symbol)
            (dom :: Domain)
            (s :: Type)
            (i :: Type)
            (o :: Type)
   . ( KnownDomain dom
     , NFDataX s )
  => Clock dom
  -- ^ 'Clock' to synchronize to
  -> Reset dom
  -> Enable dom
  -- ^ Global enable
  -> (s -> i -> (s,o))
  -- ^ Transfer function in mealy machine form: @state -> input -> (newstate,output)@
  -> s
  -- ^ Initial state
  -> (Signal dom i -> Signal dom o)
  -- ^ Synchronous sequential function with input and output matching that
  -- of the mealy machine
mealyRegNamed clk rst en f iS =
  \i -> let (s',o) = unbundle $ f <$> s <*> i
            s      = prefixName @name $ E.register clk rst en iS s'
        in  o
{-# INLINABLE mealyRegNamed #-}

-- | Createa a dff synchronizer with the first dff having prefixed name1
--   and the second dff having prefixed name
namedDualFlipFlopSynchronizer
  :: forall (name1 :: Symbol)
            (name2 :: Symbol)
            (dom1 :: Domain)
            (dom2 :: Domain)
            (a :: Type)
   . ( NFDataX a
     , KnownDomain dom1
     , KnownDomain dom2 )
  => Clock dom1
  -- ^ 'Clock' to which the incoming  data is synchronized
  -> Clock dom2
  -- ^ 'Clock' to which the outgoing data is synchronized
  -> Reset dom2
  -- ^ 'Reset' for registers on the outgoing domain
  -> Enable dom2
  -- ^ 'Enable' for registers on the outgoing domain
  -> a
  -- ^ Initial value of the two synchronization registers
  -> Signal dom1 a
  -- ^ Incoming data
  -> Signal dom2 a
  -- ^ Outgoing, synchronized, data
namedDualFlipFlopSynchronizer clk1 clk2 rst en i =
  (prefixName @name2 $ E.register clk2 rst en i)
    . (prefixName @name1 $ E.register clk2 rst en i)
    . E.unsafeSynchronizer clk1 clk2

fifoMem
  :: forall wdom rdom a addrSize
   . ( KnownDomain wdom
     , KnownDomain rdom
     , NFDataX a
     , KnownNat addrSize
     , 1 <= addrSize )
  => Clock wdom
  -> Clock rdom
  -> Enable wdom
  -> Enable rdom
  -> Signal wdom Bool
  -> Signal rdom (BitVector addrSize)
  -> Signal wdom (BitVector addrSize)
  -> Signal wdom (Maybe a)
  -> Signal rdom a
fifoMem wclk rclk wen ren full raddr waddr wdataM =
  fst $ E.trueDualPortBlockRam
    rclk wclk portA portB
 where
   portA :: Signal rdom (RamOp (2 ^ addrSize) a)
   portA = mux (fromEnable ren)
               (RamRead . unpack <$> raddr)
               (pure RamNoOp)
   portB :: Signal wdom (RamOp (2 ^ addrSize) a)
   portB = mux (fromEnable wen .&&. fmap not full .&&. fmap isJust wdataM)
               (RamWrite <$> fmap unpack waddr <*> fmap fromJustX wdataM)
               (pure RamNoOp)

readPtrCompareT
  :: KnownNat addrSize
  => ( BitVector (addrSize + 1)
     , BitVector (addrSize + 1)
     , Bool )
  -> ( BitVector (addrSize + 1)
     , Bool )
  -> ( ( BitVector (addrSize + 1)
       , BitVector (addrSize + 1)
       , Bool )
     , ( Bool
       , BitVector addrSize
       , BitVector (addrSize + 1)
       )
     )
readPtrCompareT (bin, ptr, flag) (s_ptr, inc) =
  ((bin', ptr', flag'), (flag, addr, ptr))
 where
  -- GRAYSTYLE2 pointer
  bin' = bin + boolToBV (inc && not flag)
  ptr' = (bin' `shiftR` 1) `xor` bin'
  addr = truncateB bin'

  flag' = ptr' == s_ptr

writePtrCompareT
  :: (2 <= addrSize)
  => SNat addrSize
  -> ( BitVector (addrSize + 1)
     , BitVector (addrSize + 1)
     , Bool )
  -> ( BitVector (addrSize + 1)
     , Bool )
  -> ( ( BitVector (addrSize + 1)
       , BitVector (addrSize + 1)
       , Bool )
     , ( Bool
       , BitVector addrSize
       , BitVector (addrSize + 1)
       )
     )
writePtrCompareT addrSize@SNat (bin, ptr, flag) (s_ptr, inc) =
  ((bin', ptr', flag'), (flag, addr, ptr))
 where
  -- GRAYSTYLE2 pointer
  bin' = bin + boolToBV (inc && not flag)
  ptr' = (bin' `shiftR` 1) `xor` bin'
  addr = truncateB bin

  flag' = isFull addrSize ptr' s_ptr

-- FIFO full: when next pntr == synchronized {~wptr[addrSize:addrSize-1],wptr[addrSize-2:0]}
isFull
  :: forall addrSize
   . (2 <= addrSize)
  => SNat addrSize
  -> BitVector (addrSize + 1)
  -> BitVector (addrSize + 1)
  -> Bool
isFull addrSize@SNat ptr s_ptr =
  case leTrans @1 @2 @addrSize of
    Sub Dict ->
      let a1 = SNat @(addrSize - 1)
          a2 = SNat @(addrSize - 2)
      in  ptr == (complement (slice addrSize a1 s_ptr) ++# slice a2 d0 s_ptr)

-- | Synchronizer implemented as a FIFO around a synchronous RAM. Based on the
-- design described in "Clash.Tutorial#multiclock", which is itself based on the
-- design described in <http://www.sunburst-design.com/papers/CummingsSNUG2002SJ_FIFO1.pdf>.
-- However, this FIFO uses a synchronous dual-ported RAM which, unlike those
-- designs using RAM with an asynchronous read port, is nearly guaranteed to
-- actually synthesize into one of the dual-ported RAMs found on most FPGAs.
--
-- __NB__: This synchronizer can be used for __word__-synchronization.
-- __NB__: This synchronizer will only work safely when you set up the proper
-- bus skew and maximum delay constraints inside your synthesis tool for the
-- clock domain crossings of the gray pointers.
{-# NOINLINE asyncFIFOSynchronizer #-}
asyncFIFOSynchronizer
  :: ( KnownDomain wdom
     , KnownDomain rdom
     , 2 <= addrSize
     , NFDataX a )
  => SNat addrSize
  -- ^ Size of the internally used addresses, the  FIFO contains @2^addrSize@
  -- elements.
  -> Clock wdom
  -- ^ 'Clock' to which the write port is synchronized
  -> Clock rdom
  -- ^ 'Clock' to which the read port is synchronized
  -> Reset wdom
  -> Reset rdom
  -> Enable wdom
  -> Enable rdom
  -> Signal rdom Bool
  -- ^ Read request
  -> Signal wdom (Maybe a)
  -- ^ Element to insert
  -> (Signal rdom a, Signal rdom Bool, Signal wdom Bool)
  -- ^ (Oldest element in the FIFO, @empty@ flag, @full@ flag)
asyncFIFOSynchronizer addrSize@SNat wclk rclk wrst rrst wen ren rinc wdataM =
  (rdata, rempty, wfull)
 where
  s_rptr = namedDualFlipFlopSynchronizer @"rPtr1" @"rPtr2" rclk wclk wrst wen 0 rptr
  s_wptr = namedDualFlipFlopSynchronizer @"wPtr1" @"wPtr2" wclk rclk rrst ren 0 wptr

  rdata =
    fifoMem
      wclk rclk wen ren
      wfull raddr
      waddr wdataM

  (rempty, raddr, rptr) =
    unbundle $
      mealyRegNamed @"rPtr0"
      rclk rrst ren
      readPtrCompareT
      (0, 0, True)
      (bundle (s_wptr, rinc))

  (wfull, waddr, wptr) =
    unbundle $
      mealyRegNamed @"wPtr0"
      wclk wrst wen
      (writePtrCompareT addrSize)
      (0, 0, False)
      (bundle (s_rptr, isJust <$> wdataM))

-- | Asynchronous FIFO circuit that can be used to safely cross clock domains.
-- Uses `Clash.Explicit.Prelude.asyncFIFOSynchronizer` internally.
asyncFifoC :: forall (depth     :: Nat)
                     (dataWidth :: Nat)
                     (wDom      :: Domain)
                     (rDom      :: Domain)
                     (metaType  :: Type) .
  ( KnownDomain wDom, KnownDomain rDom
  , KnownNat depth
  , 2 <= depth
  , KnownNat dataWidth
  , 1 <= dataWidth
  , NFDataX metaType)
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
asyncFifoC depth wClk wRst wEn rClk rRst rEn = fromSignals ckt where
  ckt (fwdIn, bwdIn) = (bwdOut, fwdOut) where
    (element, empty, full) = asyncFIFOSynchronizer depth wClk rClk wRst rRst wEn rEn readReq fwdIn
    -- If the FIFO is empty, we output Nothing. Else, we output the oldest element.
    fwdOut = mux empty (pure Nothing) (Just <$> element)
    -- Assert backpressure when the FIFO is full.
    bwdOut = PacketStreamS2M . not <$> full
    -- Next component is ready to read if the fifo is not empty and it does not assert backpressure.
    readReq = (not <$> empty) .&&. (_ready <$> bwdIn)
