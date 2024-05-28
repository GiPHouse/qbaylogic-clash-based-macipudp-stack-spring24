{-|
This module implements "ping" through some trickery. It makes the ASIC respond
to ICMP Echo Request packets with the correct ICMP Echo Reply packet.

Since IPv4 depends on ARP, and ARP is not implemented, the ARP phase of IP
communication needs to be bypassed. On a Linux host where the network interface
enp3s0 is connected to the ASIC, one can accomplish this as follows:

> # ip route add 10.0.0.2/32 dev enp3s0
> # ip neigh add 10.0.0.2 lladdr 52:54:00:eb:9b:d0 dev enp3s0

This establishes that peer 10.0.0.2 can be reached on enp3s0 at MAC address
52:54:00:eb:9b:d0, without doing ARP. If the network configured on enp3s0
is already the network where 10.0.0.2 resides, the first line can be
omitted.

We can now reach the ASIC from a networked Linux host with:

> $ ping 10.0.0.2

The way this is accomplished is some simple trickery.

First of all, since we want to buffer as little data as possible, we immediately
begin replying to any incoming packet with the EtherType for IPv4. During
reception, we check that the incoming packet is actually an ICMP Echo Request,
and if it turns out to be something else, we assert @_abort@ in the outgoing
@PacketStreamM2S@ so the PHY will deliberately garble the outgoing Ethernet
frame and the Ethernet peer will drop the data we generated thus far.

While we are copying input to output, we make a few changes.

In the Ethernet frame, we simply reverse source and destination MAC address.
This means the data is always sent back to whoever sent it to us. This works
even if this is a routed IP packet: in that case, we send it back to the router
and that will route it out further.

While echoing words of the IP packet, we swap source and destination IP, sending
the ICMP Echo Reply to the host that sent the Request. To be precise: once we
get to the source IP address in the header, we output our own, known IP address
as the source. Meanwhile, we keep a backlog of the 2 16-bit words that preceded
the current word. Once we get to the destination IP address while sending the
header, we output the data from this backlog, which is the source IP address of
the incoming ICMP Echo Request.

The IP header has a header checksum. However, this checksum remains the same
under swaps of aligned 16-bit words. Since the source and destination IP are
32-bit words, the checksum remains the same and we can just echo the incoming
checksum. If the incoming checksum was wrong, so will the outgoing checksum be,
postponing checksum checking to the recipient of our Echo Reply.

Once we get to the ICMP Type\/Code, we output the Type\/Code for ICMP Echo
Reply, which is 0x00_00.

ICMP uses the same checksum algorithm as the IP header. If the change in the
checksummed data is constant, it is simple to compute the correct checksum by
merely adjusting the checksum of the incoming packet: see RFC 1624. Our change
is a constant single bit flip: we change the ICMP Type/Code from 0x08_00 to
0x00_00. So we read the incoming checksum, apply two inversions, do a single
one's complement addition, and the outgoing checksum is correct if the incoming
was.

All the other words are just echoed verbatim.

While the packet comes in, we check the following values:

 - IP version nibble
 - Header length: 20 bytes; no IP options
 - Layer 3 protocol: ICMP (0x01)
 - Destination IP address: our own static IP (10.0.0.2)
 - ICMP Type/Code: ICMP Echo Request (0x08_00)

From the moment we see an incorrect value, we start asserting @_abort@ in the
outgoing stream, but we continue sending in the same fashion otherwise for
simplicity. An incoming @_abort@ is copied to the outgoing stream.

Where this approach wholly falls apart is fragmentation. For simplicity, it
does not even verify the packet is unfragmented, but only unfragmented packets
exhibit correct behaviour.

To the initial fragment of a fragmented ICMP Echo Request, you will get the
initial fragment of the reply, but no reply to the non-initial fragments.

When the ASIC receives a non-initial ICMP fragment (any ICMP, regardless of
whether it was an Echo Request), there is a small chance it will evoke a bogus
reply packet from the ASIC. If the first two bytes of the ICMP fragment are
0x08_00, it will get a garbled response from the ASIC.

A malformed incoming packet that does pass the list of checked values above will
of course lead to a malformed reply packet. However, note that the minimum
Ethernet frame is larger than the minimum valid ICMP Echo Request/Reply, so it
is really difficult to send a too short packet to the component in this module.
-}

{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language NumericUnderscores #-}
{-# language RecordWildCards #-}

module Clash.TinyTapeout.EthernetMac.IcmpEcho where

-- import Debug.Trace (trace)

-- import Clash.Annotations.BitRepresentation.Deriving
import Clash.Prelude
import Data.Maybe (isJust)

import Protocols

import Clash.Cores.Ethernet.PacketStream

type DataWidth = 2

type IpAddress = Vec 2 (Vec DataWidth (BitVector 8))

-- | Our own IP address: 10.0.0.2
stationAddress :: IpAddress
stationAddress = unconcatI $ 10 :> 0 :> 0 :> 2 :> Nil

data Fsm
  = IpVerLen
  | IpHdr1
  | L3Proto
  | IpHdr2
  | SrcIP
  | DstIP
  | IcmpTC
  | IcmpCheck
  | Trailer
  deriving (Show, Enum, Generic, NFDataX, ShowX)
-- deriveAnnotation (simpleDerivator OneHot OverlapL) [t| Fsm |]

type ShiftDepth = 2

type StateCnt = Unsigned 2

-- | The state of the Mealy machine
data IcmpEchoState = IcmpEchoState
  { fsm :: Fsm
  , dShift :: Vec ShiftDepth (Vec DataWidth (BitVector 8))
  , stateCnt :: StateCnt
  , errOut :: Bool
  }
  deriving (Show, Generic, NFDataX, ShowX)

-- | Initial state
icmpEchoIS :: IcmpEchoState
icmpEchoIS =
  IcmpEchoState
    { fsm=IpVerLen
    , stateCnt=initStateCnt $ fsm icmpEchoIS
    , dShift=ensureSpine undefined
    , errOut=False
    }

-- | Initial @stateCnt@ for the given state
initStateCnt :: Fsm -> StateCnt
initStateCnt fsm =
  case fsm of
    IpVerLen -> snatToUnsigned d0
    IpHdr1 -> snatToUnsigned d2
    L3Proto -> snatToUnsigned d0
    IpHdr2 -> snatToUnsigned d0
    SrcIP -> snatToUnsigned d1
    DstIP -> snatToUnsigned d1
    IcmpTC -> snatToUnsigned d0
    IcmpCheck -> snatToUnsigned d0
    Trailer -> undefined

-- | Mealy transition function for the ICMP Echoer
icmpEchoT
  :: forall meta
   . IcmpEchoState
     -- ^ Old state
  -> ( Maybe (PacketStreamM2S DataWidth meta)
     , PacketStreamS2M
     )
     -- ^ Tuple elements:
     --
     --   - Incoming stream
     --   - Backpressure
  -> ( IcmpEchoState
     , Maybe (PacketStreamM2S DataWidth meta)
     )
     -- ^ Tuple elements:
     --
     --   - New state
     --   - Outgoing stream
icmpEchoT s (Nothing, _) = (s, Nothing)
icmpEchoT s0@IcmpEchoState{..} (Just fwdIn, PacketStreamS2M bwdInReady) =
  (s1, fwdOut)
  where
    s1
      | not bwdInReady
      = s0
      | otherwise
      = IcmpEchoState
          { fsm=fsm1
          , stateCnt=stateCnt1
          , dShift=init dShift1
          , errOut=errOut1
          }

    -- fwdOut = trace (show fsm <> "\n" <> show fillCnt <> "\n" <> showX (map (map (unpack @(Unsigned 8))) dShift)) $
    fwdOut = Just $
      PacketStreamM2S
        { _data=dataOut1
        , _last=_last fwdIn
        , _meta=_meta fwdIn
        , _abort=errOut1
        }

    dShift1 = _data fwdIn :> dShift

    dataOut0 = dShift1 !! dPtr
    dataOut1 =
      case fsm of
        -- Output our IP address as the source IP. @stateCnt@ tells us where we
        -- are within the address. It's either one or zero.
        SrcIP | stateCntZero -> last stationAddress
              | otherwise    -> head stationAddress
        -- Output ICMP Type/Code for ICMP Echo Reply
        IcmpTC -> 0x00 :> 0x00 :> Nil
        -- Adjust checksum
        IcmpCheck -> bitCoerce cksum
        -- Echo incoming data verbatim
        _ -> dataOut0

    dPtr :: Index (ShiftDepth+1)
    dPtr =
      case fsm of
        -- We need to peek two words into the past of the incoming words (which
        -- we keep at index 2 in @dShift1@) so we output the incoming source IP
        -- address as the outgoing destination IP address.
        DstIP -> 2
        -- We just output the currently incoming word in the input stream.
        _ -> 0

    -- See RFC 1624 Computation of the Internet Checksum via Incremental Update
    --
    -- HC' = ~(C + (-m) + m')    --    [Eqn. 3]
    --     = ~(~HC + ~m + m')
    --
    -- HC = bitCoerce dataOut0      (old checksum)
    -- m  = 0x0800                  (old ICMP Type/Code)
    -- m' = 0x0000                  (new ICMP Type/Code)
    --
    -- ~m + m' = 0xf7ff + 0x0000
    --         = 0xf7ff
    cksum :: Unsigned 16
    cksum = complement $ complement (bitCoerce dataOut0) ~+~ 0xf7ff

    errOut1 =
      case fsm of
        IpVerLen -> errNow
        _ -> errOut || errNow
      where
        -- Assert error when the incoming stream errors, or when the current two
        -- bytes in the input stream do not match @checkValues@
        errNow =
          _abort fwdIn || or (zipWith badWord checkValues $ _data fwdIn)

    -- Which bytes do we want to see in the input? @Nothing@ if we don't care.
    checkValues =
      case fsm of
        -- Check IP version nibble and header length
        IpVerLen -> Just 0x45 :> Nothing :> Nil
        L3Proto -> Nothing :> Just 0x01 :> Nil
        -- Check we were the addressed party. @stateCnt@ tells us where we are
        -- within the address. It's either one or zero.
        DstIP
          | stateCntZero -> map Just $ last stationAddress
          | otherwise    -> map Just $ head stationAddress
        IcmpTC -> Just 0x08 :> Just 0x00 :> Nil
        _ -> repeat Nothing

    badWord (Just check) val = check /= val
    badWord _ _ = False

    nextFsm
      | lastIn    = IpVerLen
      | otherwise = succ fsm

    fsm1 = if trans then nextFsm else fsm

    trans
      | lastIn    = True
      | otherwise =
        case fsm of
          Trailer -> False
          _ -> stateCntZero

    -- This counter runs down to zero for the number of input words we need to
    -- stay in the current state.
    stateCnt1 =
      if trans
      then initStateCnt nextFsm
      else stateCnt - 1

    lastIn = isJust $ _last fwdIn
    stateCntZero = stateCnt == 0

icmpEchoC
  :: forall dom meta
   . HiddenClockResetEnable dom
  => Circuit (PacketStream dom 2 meta) (PacketStream dom 2 meta)
icmpEchoC = fromSignals go
  where
    -- Note that we never produce backpressure ourselves, so we just pass the
    -- backpressure from our downstream port back to the upstream.
    go (fwdIn, bwdIn) = (bwdIn, fwdOut)
      where
        fwdOut = mealy icmpEchoT icmpEchoIS $ bundle (fwdIn, bwdIn)

-- | Get a constant that is verified to fit in the data type
snatToUnsigned ::
  forall m n .
  KnownNat n =>
  CLog 2 (m+1) <= n =>
  SNat m ->
  Unsigned n
snatToUnsigned = snatToNum

-- | One's complement addition
(~+~) :: KnownNat n
      => Unsigned n
      -> Unsigned n
      -> Unsigned n
a ~+~ b = truncateB summed + resize (bitCoerce carry)
    where
        summed = a `add` b
        carry = msb summed
