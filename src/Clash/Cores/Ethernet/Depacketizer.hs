module Clash.Cores.Ethernet.Depacketizer
  ( depacketizerC
  , macDepacketizerC
  , MacAddress
  , EthernetHeader) where

import Data.Maybe

import Clash.Prelude

import Protocols

import Clash.Cores.Ethernet.PacketStream

import Control.DeepSeq (NFData)

import qualified Data.List as L


data DepacketizerState dataWidth n metaIn
  = Idle
  | Parse {
    _byte_idx :: Index n,
    _state :: Vec n (BitVector 8)
  }
  | Forward {
    _last_fragment :: PacketStreamM2S dataWidth metaIn,
    _state :: Vec n (BitVector 8)
  }
  | LastForward {
    _last_fragment :: PacketStreamM2S dataWidth metaIn,
    _state :: Vec n (BitVector 8)
  }
  deriving (Show, Generic, NFDataX)

sFunc :: forall (dataWidth :: Nat) (n :: Nat) (g :: Nat) (metaIn :: Type) (metaOut :: Type) .
  ( KnownNat dataWidth
  , KnownNat n
  , 1 <= dataWidth
  , Mod n dataWidth + g ~ dataWidth
  , BitPack metaOut
  , BitSize metaOut ~ n * 8)
  => SNat dataWidth
  -> SNat n
  -> DepacketizerState dataWidth n metaIn
  -> (Maybe (PacketStreamM2S dataWidth metaIn), PacketStreamS2M)
  -> (DepacketizerState dataWidth n metaIn, (PacketStreamS2M, Maybe (PacketStreamM2S dataWidth metaOut)))

-- A packet is incoming, so we start depacketizing. If dataWidth >= n, then depacketization is done immediately,
-- so forwarding data can start immediately next clock cycle. Else, we need to wait some more clock cycles. In this case,
-- we move to state Strip and record how many bytes are already parsed.
sFunc dataWidth n Idle (Just inp, _bwdIn) = (nextState, (PacketStreamS2M True, Nothing))
  where
    parsedBytes :: Vec n (BitVector 8)
    parsedBytes = case compareSNat n dataWidth of
      SNatLE -> take n (_data inp :: Vec (n + (dataWidth-n)) (BitVector 8))
      SNatGT -> fst (shiftInAtN (replicate n 0x00) (_data inp))

    nextState = case _last inp of
      Nothing -> case compareSNat n dataWidth of
        SNatLE -> Forward {_last_fragment = inp, _state = parsedBytes}
        SNatGT -> Parse {_byte_idx = snatToNum dataWidth, _state = parsedBytes}
      -- We do not accept Justs in this state, in this case we go to Idle.
      Just _ -> Idle

-- Further depacketization in case dataWidth < n.
sFunc dataWidth n Parse {_byte_idx = i, _state = state} (Just inp, _bwdIn) = case compareSNat n dataWidth of
  SNatLE -> undefined -- impossible branch, because we immediately go to Forward in this case
  SNatGT -> (nextState, (PacketStreamS2M True, Nothing))
    where
      overflow = satAdd SatZero i (snatToNum dataWidth) == 0
      takeMod = overflow && satAdd SatWrap i (snatToNum dataWidth) >= 1

      bytes
        = if takeMod
          then fst (shiftInAtN state (take (modSNat n dataWidth) (_data inp)))
          else fst (shiftInAtN state (_data inp))

      nextState = case _last inp of
        Nothing -> if overflow
                   then Forward {_last_fragment = inp, _state = bytes}
                   else Parse {_byte_idx = i + snatToNum dataWidth, _state = bytes}
        -- We do not accept Justs in this state, in this case we go to Idle.
        Just _ -> Idle

-- Depacketization is done, and now we are forwarding all fragments of the corresponding packet.
-- To do this, we need to store eventual data that was submitted together with the header and shift the stream.
sFunc dataWidth n Forward {_last_fragment = lastFragment, _state = state} (Just inp, bwdIn) = (nextState, (bwdIn, fwdOut))
  where
    lastData = _data lastFragment
    -- don't change state upon receiving backpressure
    nextState = if bwdIn == PacketStreamS2M False then Forward {_last_fragment = lastFragment, _state = state} else case _last lastFragment of
      Nothing -> case _last inp of
        Nothing -> Forward {_last_fragment = inp, _state = state}
        Just i -> case compareSNat n dataWidth of
          SNatLE -> if satAdd SatZero i (snatToNum (subSNat dataWidth n :: SNat (dataWidth - n))) == 0 && i /= 0
                    then LastForward {_last_fragment = inp, _state = state}
                    else Idle
          SNatGT -> if satAdd SatZero i (snatToNum (subSNat dataWidth (modSNat n dataWidth))) == 0
                    then LastForward {_last_fragment = inp, _state = state}
                    else Idle
      Just _ -> Idle
    
    fwdOut = Just PacketStreamM2S {
      _data = outData,
      _last = final,
      _meta = unpack (pack state),
      _abort = _abort inp
    }
    outData = case compareSNat n dataWidth of
      SNatLE -> oldData ++ newData
        where
          oldData = take (subSNat dataWidth n) (rotateLeftS lastData n) :: Vec (dataWidth - n) (BitVector 8)
          newData = take n (_data inp :: Vec (n + (dataWidth - n)) (BitVector 8))
      SNatGT -> case compareSNat (modSNat n dataWidth) d0 of
        SNatLE -> _data inp
        SNatGT -> oldData ++ newData
          where
            x = modSNat n dataWidth
            oldData = take (subSNat dataWidth x) (rotateLeftS lastData x)
            newData = take x (_data inp)

    final = case _last inp of
      Nothing -> Nothing
      Just i -> case compareSNat n dataWidth of
        SNatLE -> if satAdd SatZero i (snatToNum (subSNat dataWidth n)) == 0 && i /= 0
                  then Nothing
                  else Just (i + snatToNum (subSNat dataWidth n :: SNat (dataWidth - n)))
        SNatGT -> let x = modSNat n dataWidth in
                  if satAdd SatZero i (snatToNum (subSNat dataWidth x)) == 0
                  then Nothing
                  else Just (i + snatToNum (subSNat dataWidth x :: SNat (dataWidth - Mod n dataWidth)))

-- Sometimes _last has been set, but we cannot send all data immediately due to stream shifting. In this case,
-- we need to send the remainder of the data.
sFunc dataWidth n LastForward {_last_fragment = lastFragment, _state = state} (_, bwdIn) = (nextState, (PacketStreamS2M False, fwdOut))
  where
    -- don't change state upon receiving backpressure
    nextState = if bwdIn == PacketStreamS2M False then LastForward {_last_fragment = lastFragment, _state = state} else Idle
    x = modSNat n dataWidth
    i = fromJust (_last lastFragment)
    lastData = _data lastFragment
    fwdOut = Just PacketStreamM2S {
      _data = case compareSNat n dataWidth of
        SNatLE -> (take (subSNat dataWidth n) (rotateLeftS lastData n) :: Vec (dataWidth-n) (BitVector 8)) ++ replicate n 0x00
        SNatGT -> take (subSNat dataWidth x) (rotateLeftS lastData x) ++ replicate x 0x00,
      _last = case compareSNat n dataWidth of
        SNatLE -> Just (satAdd SatWrap i (snatToNum (subSNat dataWidth n :: SNat (dataWidth-n))))
        SNatGT -> Just (satAdd SatWrap i (snatToNum (subSNat dataWidth x))),
      _meta = unpack (pack state),
      _abort = False
    }

sFunc _ _ s (Nothing, bwdIn) = (s, (bwdIn, Nothing))


-- | Generic component that depacketizes the first `n` bytes of the incoming PacketStream,
-- that is, it reads and strips `n` bytes from the head of the stream and puts these in _meta.
-- Works for any `metaOut` which implements BitPack and which BitSize is a multiple of 8 bits.
depacketizerC :: forall (dom :: Domain) (dataWidth :: Nat) (n :: Nat) (g :: Nat) (metaIn :: Type) (metaOut :: Type).
  ( KnownDomain dom
  , HiddenClockResetEnable dom
  , KnownNat dataWidth
  , KnownNat n
  , 1 <= dataWidth
  , 1 <= n
  , Mod n dataWidth + g ~ dataWidth
  , NFDataX metaIn
  , BitPack metaOut
  , BitSize metaOut ~ n * 8)
  => SNat dataWidth
  -> SNat n
  -> Circuit (PacketStream dom dataWidth metaIn) (PacketStream dom dataWidth metaOut)
depacketizerC dataWidth n = forceResetSanity |> (fromSignals $ mealyB (sFunc dataWidth n) s0)
  where
    s0 = Idle


newtype MacAddress = MacAddress (Vec 6 (BitVector 8))
  deriving (Show, ShowX, Eq, Generic, BitPack, NFDataX, NFData)

data EthernetHeader = EthernetHeader {
  _mac_dst :: MacAddress,
  _mac_src :: MacAddress,
  _ether_type :: BitVector 16
} deriving (Show, ShowX, Eq, Generic, BitPack, NFDataX, NFData)

macDepacketizerC :: forall (dom :: Domain) (dataWidth :: Nat) (g :: Nat) .
  ( KnownDomain dom
  , HiddenClockResetEnable dom
  , KnownNat dataWidth
  , 1 <= dataWidth
  , Mod 14 dataWidth + g ~ dataWidth)
  => SNat dataWidth
  -> Circuit (PacketStream dom dataWidth ()) (PacketStream dom dataWidth EthernetHeader)
macDepacketizerC dataWidth = depacketizerC dataWidth d14
















fwdIn9 :: [Maybe (PacketStreamM2S 9 ())]
fwdIn9 = [
  Nothing,
  Nothing,
  Nothing,
  Just (PacketStreamM2S (0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> Nil) Nothing () False)
  , Just (PacketStreamM2S (0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> Nil) Nothing () False)
  , Just (PacketStreamM2S (0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> Nil) Nothing () False)
  , Just (PacketStreamM2S (0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> Nil) Nothing () False)
  , Just (PacketStreamM2S (0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> Nil) Nothing () False)
  , Just (PacketStreamM2S (0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> Nil) Nothing () False)
  , Just (PacketStreamM2S (0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> Nil) (Just 0) () False)]
  L.++ L.repeat Nothing

bwdIn9 :: [PacketStreamS2M]
bwdIn9 = fmap PacketStreamS2M (L.repeat True)


fwdIn14 :: [Maybe (PacketStreamM2S 14 ())]
fwdIn14 = [
  Nothing,
  Nothing,
  Nothing,
  Just (PacketStreamM2S (0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00:> 0x00:> 0x00:> 0x00:> 0x00:> Nil) Nothing () False)
  , Just (PacketStreamM2S (0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :>0x00:> 0x00:> 0x00:> 0x00:> 0x00:> Nil) Nothing () False)
  , Just (PacketStreamM2S (0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :>0x00:> 0x00:> 0x00:> 0x00:> 0x00:> Nil) Nothing () False)
  , Just (PacketStreamM2S (0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :>0x00:> 0x00:> 0x00:> 0x00:> 0x00:> Nil) Nothing () False)
  , Just (PacketStreamM2S (0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :>0x00:> 0x00:> 0x00:> 0x00:> 0x00:> Nil) Nothing () False)
  , Just (PacketStreamM2S (0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :>0x00:> 0x00:> 0x00:> 0x00:> 0x00:> Nil) Nothing () False)
  , Just (PacketStreamM2S (0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :>0x00:> 0x00:> 0x00:> 0x00:> 0x00:> Nil) (Just 0) () False)]
  L.++ L.repeat Nothing

bwdIn14 :: [PacketStreamS2M]
bwdIn14 = fmap PacketStreamS2M (L.repeat True)

clk :: Clock System
clk = systemClockGen

rst :: Reset System
rst = systemResetGen

en :: Enable System
en = enableGen

fwdOut9 :: Signal System (Maybe (PacketStreamM2S 9 EthernetHeader))
bwdOut9 :: Signal System PacketStreamS2M
(bwdOut9, fwdOut9) = toSignals ckt (fromList fwdIn9, fromList bwdIn9)
  where ckt = exposeClockResetEnable (macDepacketizerC d9) clk rst en


fwdOut14 :: Signal System (Maybe (PacketStreamM2S 14 EthernetHeader))
bwdOut14 :: Signal System PacketStreamS2M
(bwdOut14, fwdOut14) = toSignals ckt (fromList fwdIn14, fromList bwdIn14)
  where ckt = exposeClockResetEnable (macDepacketizerC d14) clk rst en