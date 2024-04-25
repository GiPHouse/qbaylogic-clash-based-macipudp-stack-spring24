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
  :: forall (dom :: Domain) (n :: Nat) m
   . ( HiddenClockResetEnable dom
     , KnownNat n
     , 1 <= n
     , n ~ 2 * m
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
  :: forall dom n m
   . ( HiddenClockResetEnable dom
     , KnownNat n
     , 1 <= n
     , n ~ m * 2
     )
  => Circuit (PacketStream dom n EthernetHeader) (PacketStream dom n EthernetHeader)
verifyChecksum = Circuit $ mealyB go (0, 0)
  where
    go
      :: (BitVector 16, Index (20 `DivRU` n))
      -> (Maybe (PacketStreamM2S n EthernetHeader), PacketStreamS2M)
      -> ((BitVector 16, Index (20 `DivRU` n)), (PacketStreamS2M, Maybe (PacketStreamM2S n EthernetHeader)))
    go s (Nothing, bwd) = (s, (bwd, Nothing))
    go (acc, i) (Just fwdIn, PacketStreamS2M inReady) = (s', (PacketStreamS2M inReady, Just fwdOut))
      where 
        containsData = i == maxBound
        dataIdx = natToNum @(20 `Mod` n)
        header = imap (\j x -> if containsData || j >= dataIdx then 0 else x) $ _data fwdIn
        acc' = fold onesComplementAdd (acc :> bitCoerce header)
        i' = if i < maxBound then i + 1 else maxBound
        s' = if isJust (_last fwdIn) && inReady then (0, 0) else (acc', i')
        fwdOut = if containsData then fwdIn { _abort = _abort fwdIn || acc' /= 0 } else fwdIn
