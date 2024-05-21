{-|
Module      : Clash.Cores.Ethernet.IPDepacketizer
Description : Strips the IP header from packets
-}
module Clash.Cores.Ethernet.IPDepacketizer
  ( ipDepacketizerC
  , ipDepacketizerLiteC
  ) where

import Clash.Cores.Ethernet.Depacketizer ( depacketizerC )
import Clash.Cores.Ethernet.EthernetTypes
import Clash.Cores.Ethernet.InternetChecksum
import Clash.Cores.Ethernet.PacketStream
import Clash.Cores.IP.IPv4Types
import Clash.Prelude
import Data.Maybe
import Data.Type.Equality
import Protocols


-- | Parses the IPv4 header. Does not support parsing options in the header.
-- If the checksum is invalid or options are given, the abort bit is set.
ipDepacketizerC
  :: forall (dom :: Domain) (n :: Nat)
   . ( HiddenClockResetEnable dom
     , KnownNat n
     , 1 <= n
     , 20 `Mod` n <= n
     )
  => Circuit (PacketStream dom n EthernetHeader) (PacketStream dom n IPv4Header)
ipDepacketizerC = verifyChecksum |> depacketizerC const |> verifyLength
  where
    verifyLength = Circuit $ \(fwdIn, bwdIn) -> (bwdIn, (go <$>) <$> fwdIn)
    go p =
      let
       header = _meta p
       abort =
         _ipv4Version header /= 4 ||
         _ipv4Ihl header /= 5 ||
         _ipv4FlagReserved header ||
         _ipv4FlagMF header || _ipv4FragmentOffset header /= 0 -- drop fragmented packets
      in p {_abort = _abort p || abort}

-- | Version of `ipDepacketizerC` that only keeps some of the IPv4 header fields.
ipDepacketizerLiteC
  :: forall (dom :: Domain) (n :: Nat)
   . ( HiddenClockResetEnable dom
     , KnownNat n
     , 1 <= n
     , 20 `Mod` n <= n
     )
  => Circuit (PacketStream dom n EthernetHeader) (PacketStream dom n IPv4HeaderLite)
ipDepacketizerLiteC = ipDepacketizerC |> toLiteC

data VerifyChecksumS n
  = Check (BitVector 16) (Index (1 + 20 `Div` n)) (BitVector 8)
  -- ^ Checking. Contains accumulator, remaining bytes, and byte from previous
  -- packet in case of odd data widths
  | Forward Bool
  -- ^ Forwarding. Bool is True if checksum was invalid
  deriving (Generic, NFDataX)

-- | Verifies the internet checksum of the first 20 bytes of each packet. Sets
-- the abort bit from byte 21 onwards if the checksum is invalid. Data is left
-- intact.
verifyChecksum
  :: forall dom n
   . ( HiddenClockResetEnable dom
     , KnownNat n
     , 1 <= n
     , 20 `Mod` n <= n
     )
  => Circuit (PacketStream dom n EthernetHeader) (PacketStream dom n EthernetHeader)
verifyChecksum = case sameNat (SNat @n) (SNat @(2 * (n `Div` 2) + n `Mod` 2)) of
    Just Refl -> Circuit $ mealyB go (Check 0 0 undefined)
    _ -> errorX "ipDepacketizerC: absurd in verifyChecksum: 2 * (n `Div` 2) + n `Mod` 2 not equal to n"
  where
    go :: ( 2 * (n `Div` 2) + n `Mod` 2 ~ n )
      => VerifyChecksumS n
      -> (Maybe (PacketStreamM2S n EthernetHeader), PacketStreamS2M)
      -> (VerifyChecksumS n, (PacketStreamS2M, Maybe (PacketStreamM2S n EthernetHeader)))
    go (Forward invalid) (mp, bwd) = (Forward invalid, (bwd, (\p -> p {_abort = _abort p || invalid}) <$> mp))
    go s@(Check {}) (Nothing, bwd) = (s, (bwd, Nothing))
    go s@(Check acc i byte) (Just fwdIn, PacketStreamS2M inReady) = (s', (PacketStreamS2M inReady, Just fwdOut))
      where
        containsData = i == maxBound

        -- Set all data bytes to zero
        (dataLo, dataHi0) = splitAtI @(20 `Mod` n) @(n - 20 `Mod` n) $ _data fwdIn
        dataHi1 = if containsData then repeat 0 else dataHi0
        header = dataLo ++ dataHi1

        -- We verify that 2 * n Div 2 + n Mod 2 = n, and distinguish between
        -- even and odd data widths. For odd data widths, we have to save the
        -- extra byte in even packets and use it in the odd packets.
        (acc', byte') =
          case ( sameNat (SNat @(n `Mod` 2)) d0
               , sameNat (SNat @(n `Mod` 2)) d1
               ) of
            (Just Refl, _) -> -- n mod 2 = 0
              let xs :: Vec (n `Div` 2) (BitVector 16)
                  xs = bitCoerce header
               in (fold onesComplementAdd (acc :> xs), undefined)
            (_, Just Refl) -> -- n mod 2 = 1
              let xs :: Vec (n `Div` 2 + 1) (BitVector 16)
                  xs = bitCoerce $ if even i then init header :< 0 :< 0 else byte :> header
               in (fold onesComplementAdd (acc :> xs), last header)
            _ -> errorX "ipDepacketizerC: absurd in verifyChecksum: n `Mod` 2 not equal to 0 or 1"

        invalid = acc' /= 0xFFFF -- Note that we haven't taken the complement
        i' = satSucc SatBound i
        s'
          | not inReady = s
          | isJust (_last fwdIn) = Forward invalid
          | otherwise = Check acc' i' byte'

        fwdOut = if containsData then fwdIn { _abort = _abort fwdIn || invalid } else fwdIn
