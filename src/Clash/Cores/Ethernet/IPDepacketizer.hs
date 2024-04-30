{-# language RecordWildCards #-}

{-|
Module      : Clash.Cores.Ethernet.IPDepacketizer
Description : Strips the IP header from packets
-}
module Clash.Cores.Ethernet.IPDepacketizer
  ( ipDepacketizerC
  , IPv4Header(..)
  , IPv4HeaderLite(..)
  , toLite
  ) where

import Clash.Prelude
import Clash.Cores.Ethernet.PacketStream
import Clash.Cores.Ethernet.EthernetTypes
import Clash.Cores.Ethernet.InternetChecksum
import Protocols
import Control.DeepSeq ( NFData )
import Clash.Cores.Ethernet.Depacketizer (depacketizerC)
import qualified Data.Bifunctor as B
import Data.Tuple
import Data.Maybe
import Data.Type.Equality

type IPv4Address = BitVector 32

-- | (Almost) full IPv4 header. Does not contain options field.
data IPv4Header = IPv4Header
  { _ipv4Version :: BitVector 4
  , _ipv4Ihl :: Unsigned 4
  , _ipv4Dscp :: BitVector 6
  , _ipv4Ecn :: BitVector 2
  , _ipv4Length :: Unsigned 16
  , _ipv4Id :: BitVector 16
  , _ipv4Flags :: BitVector 3
  , _ipv4FragmentOffset :: BitVector 13
  , _ipv4Ttl :: Unsigned 8
  , _ipv4Protocol :: Unsigned 8
  , _ipv4Checksum :: BitVector 16
  , _ipv4Source :: IPv4Address
  , _ipv4Destination :: IPv4Address
  } deriving (Show, ShowX, Eq, Generic, BitPack, NFDataX, NFData)

-- | Partial IPv4 header.
data IPv4HeaderLite = IPv4HeaderLite
  { _ipv4lSource :: IPv4Address
  , _ipv4lDestination :: IPv4Address
  } deriving (Show, ShowX, Eq, Generic, BitPack, NFDataX, NFData)

toLite :: IPv4Header -> IPv4HeaderLite
toLite IPv4Header {..} = IPv4HeaderLite _ipv4Source _ipv4Destination

-- | Applies `toLite` to the metadata of all packets
toLiteC :: Circuit (PacketStream dom n IPv4Header) (PacketStream dom n IPv4HeaderLite)
toLiteC = Circuit (swap . unbundle . go . bundle)
  where
    go = fmap $ B.first $ fmap $ fmap toLite

-- | Parses the IPv4 header. Does not support parsing options in the header.
-- If the checksum is invalid or options are given, the abort bit is set.
ipDepacketizerC
  :: forall (dom :: Domain) (n :: Nat)
   . ( HiddenClockResetEnable dom
     , KnownNat n
     , 1 <= n
     )
  => Circuit (PacketStream dom n EthernetHeader) (PacketStream dom n IPv4HeaderLite)
ipDepacketizerC = verifyChecksum |> depacketizerC const |> verifyLength |> toLiteC
  where
    verifyLength = Circuit $ \(fwdIn, bwdIn) -> (bwdIn, (go <$>) <$> fwdIn)
    go p = p {_abort = _abort p || _ipv4Ihl (_meta p) /= 5}

-- | Verifies the internet checksum of the first 20 bytes of each packet. Sets
-- the abort bit from byte 21 onwards if the checksum is invalid. Data is left
-- intact.
verifyChecksum
  :: forall dom n
   . ( HiddenClockResetEnable dom
     , KnownNat n
     , 1 <= n
     )
  => Circuit (PacketStream dom n EthernetHeader) (PacketStream dom n EthernetHeader)
verifyChecksum = Circuit $ mealyB go (0, 0, undefined)
  where
    go :: (BitVector 16, Index (1 + 20 `Div` n), BitVector 8)
      -- Accumulator for checksum, packet counter, extra byte for odd data widths
      -> (Maybe (PacketStreamM2S n EthernetHeader), PacketStreamS2M)
      -> ((BitVector 16, Index (1 + 20 `Div` n), BitVector 8), (PacketStreamS2M, Maybe (PacketStreamM2S n EthernetHeader)))
    go s (Nothing, bwd) = (s, (bwd, Nothing))
    go s@(acc, i, byte) (Just fwdIn, PacketStreamS2M inReady) = (s', (PacketStreamS2M inReady, Just fwdOut))
      where
        containsData = i == maxBound
        dataIdx = natToNum @(20 `Mod` n)

        -- Set all data bytes to zero
        header = imap (\j x -> if containsData && j >= dataIdx then 0 else x) $ _data fwdIn

        -- We verify that 2 * n Div 2 + n Mod 2 = n, and distinguish between
        -- even and odd data widths. For odd data widths, we have to save the
        -- extra byte in even packets and use it in the odd packets.
        (acc', byte') =
          case ( sameNat (SNat @n) (SNat @(2 * (n `Div` 2) + n `Mod` 2))
               , sameNat (SNat @(n `Mod` 2)) d0
               , sameNat (SNat @(n `Mod` 2)) d1
               ) of
            (Just Refl, Just Refl, _) -> -- n mod 2 = 0
              let xs :: Vec (n `Div` 2) (BitVector 16)
                  xs = bitCoerce header
               in (fold onesComplementAdd (acc :> xs), undefined)
            (Just Refl, _, Just Refl) -> -- n mod 2 = 1
              let xs :: Vec (n `Div` 2 + 1) (BitVector 16)
                  xs = bitCoerce $ if even i then init header :< 0 :< 0 else byte :> header
               in (fold onesComplementAdd (acc :> xs), last header)
            _ -> errorX "ipDepacketizerC: absurd in verifyChecksum: 2 * (n `Div` 2) + n `Mod` 2 not equal to n, or n `Mod` 2 not 0 or 1"

        i' = satSucc SatSymmetric i
        s'
          | not inReady = s
          | isJust (_last fwdIn) = (0, 0, undefined)
          | otherwise = (acc', i', byte')
        fwdOut = if containsData then fwdIn { _abort = _abort fwdIn || acc' /= 0 } else fwdIn
