{-# language FlexibleContexts #-}
{-# language MultiParamTypeClasses #-}
{-# language RecordWildCards #-}
{-# language ScopedTypeVariables #-}

module Clash.Cores.Ethernet.FcsInserter (
  fcsInserterC
) where

-- crc
import Clash.Cores.Crc
import Clash.Cores.Crc.Catalog

-- ethernet
import Clash.Cores.Ethernet.PacketStream
import Clash.Cores.Ethernet.Util

-- prelude
import Clash.Prelude

-- proxy
import Data.Data ( Proxy(Proxy) )

-- maybe
import Data.Maybe

-- vector
import Clash.Sized.Vector.Extra

-- protocols
import Protocols


toCRCInput
  :: KnownNat dataWidth
  => PacketStreamM2S dataWidth ()
  -> (Index dataWidth, Vec dataWidth (BitVector 8))
toCRCInput (PacketStreamM2S{..}) = (fromMaybe maxBound _last, _data)

fcsHelperT
  :: forall dataWidth
  . KnownNat dataWidth
  => 1 <= dataWidth
  => FcsInserterState dataWidth
  -> ( Vec 4 (BitVector 8)
     , Maybe (PacketStreamM2S dataWidth ())
     , PacketStreamS2M)
  -> ( FcsInserterState dataWidth
     , ( Maybe (PacketStreamM2S dataWidth ())
       , Bool))
fcsHelperT (FcsCopy Nothing) ( _, fwdIn, _) = (FcsCopy fwdIn, (Nothing, True))

fcsHelperT st@(FcsCopy (Just cache@(PacketStreamM2S{..}))) (ethCrcBytes, fwdIn, PacketStreamS2M readyIn)
  = (nextSt, (Just fwdOut, readyIn))
  where
    (combined, leftover) = splitAtI $ appendVec (fromJust _last) _data ethCrcBytes

    nextLast i = case compareSNat d5 (SNat @dataWidth) of
      SNatLE -> toMaybe (i < natToNum @(dataWidth - 4)) $ i + 4
      _ -> Nothing

    insertCrc = nextLast <$> _last

    fwdOut = case insertCrc of
      Just l -> cache { _data = combined, _last = l }
      Nothing -> cache

    nextStIfReady = if maybe True isJust insertCrc
      then FcsCopy fwdIn
      else FcsInsert
        { _aborted = _abort
        , _cachedFwd = fwdIn
        -- Since we know we are in a case where we are not transmitting the entire CRC out
        -- it's guaranteed that dataWidth - 4 <= lastIdx <= dataWidth - 1
        -- This means we don't need to look at entire state space of the index.
        -- Only the last 2 bits matter. But since dataWidth might not be 4 byte
        -- aligned we need to wrapping subtract Mod dataWidth 4 to align the index.
        -- Normally wrapping subtract is relatively expensive but since 4
        -- is a power of two we get it for free. But it means we have to do
        -- arithmetic with BitVector/Unsigned type and not index.
        --
        -- We could go even further beyond and just pass through the last 2 bits without
        -- correction and handle that in `FcsInsert`.
        , _valid = unpack $ resize (pack $ fromJustX _last) - natToNum @(Mod dataWidth 4)
        , _cachedCrc = leftover
        }

    nextSt = if readyIn then nextStIfReady else st

fcsHelperT st@(FcsInsert{..}) (_, _, PacketStreamS2M readyIn) = (nextSt, (Just dataOut, False))
  where
    finished = _valid <= natToNum @(Min (dataWidth - 1) 3) 
    (outBytes, nextBytes) = splitAtI $ _cachedCrc ++ repeat 0
    dataOut = PacketStreamM2S
      { _data = outBytes
      , _last = toMaybe finished $ resize _valid
      , _meta = ()
      , _abort = _aborted
      }

    nextStIfReady =
      if finished
        then FcsCopy _cachedFwd
        else st
          { _valid =  _valid - natToNum @dataWidth
          , _cachedCrc = nextBytes
          }

    nextSt = if readyIn then nextStIfReady else st

-- | States of the FcsInserter
data FcsInserterState dataWidth
  = FcsCopy
      { _cachedFwd :: Maybe (PacketStreamM2S dataWidth ()) }
  | FcsInsert
      { _aborted :: Bool
      , _cachedFwd :: Maybe (PacketStreamM2S dataWidth ())
      , _valid :: Index 4
      -- ^ how many bytes of _cachedCrc are valid
      , _cachedCrc :: Vec 4 (BitVector 8)
      }
  deriving (Show, Generic, NFDataX)

-- | fcsInserter
fcsInserter
  :: forall (dom :: Domain) (dataWidth :: Nat)
  .  HiddenClockResetEnable dom
  => KnownNat dataWidth
  => HardwareCrc Crc32_ethernet 8 dataWidth
  => ( Signal dom (Maybe (PacketStreamM2S dataWidth ()))
     , Signal dom PacketStreamS2M
     )
  -> ( Signal dom PacketStreamS2M
     , Signal dom (Maybe (PacketStreamM2S dataWidth ()))
     )
fcsInserter (fwdIn, bwdIn) = (bwdOut, fwdOut)
  where
    fwdInX = fromJustX <$> fwdIn
    crcInX = toCRCInput <$> fwdInX
    transferOccured = ready .&&. isJust <$> fwdIn
    crcIn = toMaybe <$> transferOccured <*> crcInX

    firstFragment = regEn True transferOccured $ isJust . _last <$> fwdInX
    ethCrc = crcEngine @dom (Proxy @Crc32_ethernet) firstFragment crcIn
    ethCrcBytes = reverse . unpack <$> ethCrc

    bwdOut = PacketStreamS2M <$> ready

    (fwdOut, ready) = mealyB fcsHelperT (FcsCopy Nothing) (ethCrcBytes, fwdIn, bwdIn)


-- | fcsInserter circuit
fcsInserterC
  :: forall (dom :: Domain) (dataWidth :: Nat)
  .  KnownDomain dom
  => KnownNat dataWidth
  => HiddenClockResetEnable dom
  => HardwareCrc Crc32_ethernet 8 dataWidth
  => Circuit
    (PacketStream dom dataWidth ())
    (PacketStream dom dataWidth ())
fcsInserterC = forceResetSanity |> fromSignals fcsInserter


