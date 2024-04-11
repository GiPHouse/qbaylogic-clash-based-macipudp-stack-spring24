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

-- A packet is incoming, so we check if the preamble matches. If dataWidth >= 8, then the full preamble can be validated immediately,
-- so forwarding data can start immediately next clock cycle. Else, we need to wait some more clock cycles. In this case,
-- we move to state Strip and record how many bytes of the preamble are already validated.
sFunc dataWidth n Idle (Just inp, _bwdIn) = (nextState, (PacketStreamS2M True, Nothing))
  where
    parsedBytes :: Vec n (BitVector 8)
    parsedBytes = case compareSNat n dataWidth of
      SNatLE -> take n (_data inp :: Vec (n + (dataWidth-n)) (BitVector 8))
      SNatGT -> fst (shiftInAtN (replicate n 0x00) (_data inp))

    nextState = case compareSNat n dataWidth of
        SNatLE -> if isJust (_last inp)
                  then LastForward {_last_fragment = inp, _state = parsedBytes}
                  else Forward {_last_fragment = inp, _state = parsedBytes}
        SNatGT -> Parse {_byte_idx = snatToNum dataWidth, _state = parsedBytes}

-- Further validation of the preamble in case dataWidth < 8.
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

      nextState
        | overflow = if isJust (_last inp)
                     then LastForward {_last_fragment = inp, _state = bytes}
                     else Forward {_last_fragment = inp, _state = bytes}
        | otherwise = Parse {_byte_idx = i + snatToNum dataWidth, _state = bytes}

-- The preamble was valid, and now we are forwarding all fragments of the corresponding packet.
-- To do this, we need to store eventual data that was submitted together with the preamble and shift the stream.
sFunc dataWidth n Forward {_last_fragment = lastFragment, _state = state} (Just inp, bwdIn) = (nextState, (bwdIn, fwdOut))
  where
    -- todo maybe the problem is backpressure somehow?
    -- e.g. changing state when receving backpressure
    lastData = _data lastFragment
    nextState = case _last lastFragment of
      Nothing -> case _last inp of
        Nothing -> Forward {_last_fragment = inp, _state = state}
        Just i -> case compareSNat n dataWidth of
          SNatLE -> if satAdd SatZero i (snatToNum (subSNat dataWidth n :: SNat (dataWidth - n))) == 0
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
        SNatLE -> if satAdd SatZero i (snatToNum (subSNat dataWidth n)) == 0
                  then Nothing
                  else Just (i + snatToNum (subSNat dataWidth n :: SNat (dataWidth - n)))
        SNatGT -> let x = modSNat n dataWidth in
                  if satAdd SatZero i (snatToNum (subSNat dataWidth x)) == 0
                  then Nothing
                  else Just (i + snatToNum (subSNat dataWidth x :: SNat (dataWidth - Mod n dataWidth)))

-- Sometimes _last has been set, but we cannot send all data immediately due to stream shifting.
sFunc dataWidth n LastForward {_last_fragment = lastFragment, _state = state} (_, _) = (Idle, (PacketStreamS2M False, fwdOut))
  where
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

sFunc _ _ _ (Nothing, bwdIn) = (Idle, (bwdIn, Nothing))


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
depacketizerC dataWidth n = fromSignals $ mealyB (sFunc dataWidth n) s0
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
  --Just (PacketStreamM2S (0x55 :> 0x55 :> 0x55 :> 0x55 :> 0x55 :> 0x55 :> 0x55 :> 0xD5 :> 0x00 :> Nil) (Just 8) () False)
  --, Just (PacketStreamM2S (0x01 :> 0x02 :> 0x03 :> 0x04 :> 0x05 :> 0x06 :> 0x07 :> 0x08 :> 0x09 :> Nil) Nothing () False)
  --, Just (PacketStreamM2S (0x0A :> 0x0B :> 0x0C :> 0x0D :> 0x0E :> 0x0F :> 0x10 :> 0x11 :> 0x12 :> Nil) Nothing () False)
  --, Just (PacketStreamM2S (0x13 :> 0x14 :> 0x15 :> 0x16 :> 0x17 :> 0x18 :> 0x19 :> 0x1A :> 0x1B :> Nil) (Just 8) () False)]
  Just (PacketStreamM2S (0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> Nil) Nothing () False)
  , Just (PacketStreamM2S (0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> Nil) Nothing () False)
  , Just (PacketStreamM2S (0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> Nil) Nothing () False)
  , Just (PacketStreamM2S (0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> Nil) Nothing () False)
  , Just (PacketStreamM2S (0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> Nil) Nothing () False)
  , Just (PacketStreamM2S (0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> Nil) (Just 0) () False)]
  L.++ L.repeat Nothing

bwdIn9 :: [PacketStreamS2M]
bwdIn9 = fmap PacketStreamS2M (L.repeat True)

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