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

-- Manually add an ARP entry on linux
-- arp -s 10.0.0.2 5E:A4:4E:F4:21:06
stationAddress :: IpAddress
stationAddress = bitCoerce $ (10 :: BitVector 8) :> 0 :> 0 :> 2 :> Nil

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

data IcmpEchoState = IcmpEchoState
  { fsm :: Fsm
  , dShift :: Vec ShiftDepth (Vec DataWidth (BitVector 8))
  , stateCnt :: StateCnt
  , errOut :: Bool
  }
  deriving (Show, Generic, NFDataX, ShowX)

icmpEchoIS :: IcmpEchoState
icmpEchoIS =
  IcmpEchoState
    { fsm=IpVerLen
    , stateCnt=initStateCnt $ fsm icmpEchoIS
    , dShift=ensureSpine undefined
    , errOut=False
    }

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

icmpEchoT
  :: forall meta
   . IcmpEchoState
  -> ( Maybe (PacketStreamM2S DataWidth meta)
     , PacketStreamS2M
     )
  -> ( IcmpEchoState
     , Maybe (PacketStreamM2S DataWidth meta)
     )
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
        SrcIP | stateCntZero -> last stationAddress
              | otherwise    -> head stationAddress
        IcmpTC -> 0x00 :> 0x00 :> Nil
        IcmpCheck -> bitCoerce $ cksum
        _ -> dataOut0

    dPtr :: Index (ShiftDepth+1)
    dPtr =
      case fsm of
        DstIP -> 2
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
    cksum =
      complement $
        (complement $ bitCoerce dataOut0) ~+~ 0xf7ff

    errOut1 =
      case fsm of
        IpVerLen -> errNow
        _ -> errOut || errNow
      where
        errNow =
          _abort fwdIn || or (zipWith badWord checkValues $ _data fwdIn)

    checkValues =
      case fsm of
        IpVerLen -> Just 0x45 :> Nothing :> Nil
        L3Proto -> Nothing :> Just 0x01 :> Nil
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
    go (fwdIn, bwdIn) = (bwdIn, fwdOut)
      where
        fwdOut = mealyB icmpEchoT icmpEchoIS (fwdIn, bwdIn)

-- Get a constant that is verified to fit in the data type
snatToUnsigned ::
  forall m n .
  KnownNat n =>
  CLog 2 (m+1) <= n =>
  SNat m ->
  Unsigned n
snatToUnsigned = snatToNum

-- One's complement addition.
(~+~) :: KnownNat n
      => Unsigned n
      -> Unsigned n
      -> Unsigned n
a ~+~ b = truncateB summed + resize (bitCoerce carry)
    where
        summed = a `add` b
        carry = msb summed
