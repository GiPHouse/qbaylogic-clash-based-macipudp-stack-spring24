{-# language FlexibleContexts #-}
{-# language MultiParamTypeClasses #-}
{-# language RecordWildCards #-}
{-# language ScopedTypeVariables #-}

module Clash.Cores.Ethernet.FcsInserter (
  fcsInserter
  , fcsInserterC
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

-- protocols
import Protocols


fragment2bv
  :: KnownNat dataWidth
  => PacketStreamM2S dataWidth ()
  -> (Index dataWidth, Vec dataWidth (BitVector 8))
fragment2bv (PacketStreamM2S{..}) = (fromMaybe maxBound _last, _data)

-- | Like 'take' but uses a 'Data.Type.Ord.<=' constraint
takeLe
  :: forall (n :: Nat)
            (m :: Nat)
            a
   . n <= m
  => SNat n
  -- ^ How many elements to take
  -> Vec m a
  -- ^ input vector
  -> Vec n a
takeLe SNat vs = leToPlus @n @m $ takeI vs

-- | Take the first 'valid' elements of 'xs', append 'ys', then pad with 0s
appendVec
  :: forall n m a
   . KnownNat n
  => Num a
  => Index n
  -> Vec n a
  -> Vec m a
  -> Vec (n + m) a
appendVec valid xs ys = results !! valid
  where
    go :: forall l. SNat l -> Vec (n + m) a
    go l@SNat = let f = addSNat l d1 in case compareSNat f (SNat @n) of
                  SNatLE -> takeLe (addSNat l d1) xs ++ ys ++ extra
                   where
                     extra :: Vec (n - (l + 1)) a
                     extra = repeat 0
                  _ -> error "appendVec: Absurd"
    results = smap (\s _ -> go s) xs

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
    crcInX = fragment2bv <$> fwdInX
    transferOccured = ready .&&. isJust <$> fwdIn
    crcIn = toMaybe <$> transferOccured <*> crcInX

    firstFragment = regEn True transferOccured $ isJust . _last <$> fwdInX
    ethCrc = crcEngine @dom (Proxy @Crc32_ethernet) firstFragment crcIn
    ethCrcBytes = reverse . unpack <$> ethCrc

    bwdOut = PacketStreamS2M <$> ready

    (fwdOut, ready) = fcsHelper (ethCrcBytes, fwdIn, bwdIn)


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

fcsHelper
  :: forall (dom :: Domain) (dataWidth :: Nat)
  .  HiddenClockResetEnable dom
  => KnownNat dataWidth
  => ( Signal dom (Vec 4 (BitVector 8))
     , Signal dom (Maybe (PacketStreamM2S dataWidth ()))
     , Signal dom PacketStreamS2M)
  -> ( Signal dom (Maybe (PacketStreamM2S dataWidth ()))
     , Signal dom Bool
     )
fcsHelper = mealyB fcsHelperT (FcsCopy Nothing)

fcsHelperT
  :: forall dataWidth
  . KnownNat dataWidth
  => FcsInserterState dataWidth
  -> ( Vec 4 (BitVector 8)
     , Maybe (PacketStreamM2S dataWidth ())
     , PacketStreamS2M)
  -> ( FcsInserterState dataWidth
     , ( Maybe (PacketStreamM2S dataWidth ())
       , Bool))
fcsHelperT (FcsCopy Nothing) ( _, fwdIn, _) = (FcsCopy fwdIn, (Nothing, True))

fcsHelperT
  currSt@(FcsCopy (Just cache@(PacketStreamM2S{..})))
  (ethCrcBytes, fwdIn, PacketStreamS2M readyIn)
  = (nextSt, (dataOut, readyIn))
  where
    validIdx = fromMaybe maxBound _last
    (combined, leftover) = splitAt (SNat @dataWidth) $ appendVec (fromJust _last) _data ethCrcBytes

    insertAtOnce = case compareSNat d4 (SNat @dataWidth) of
      SNatLE -> fromEnum validIdx + 4 <=fromEnum (maxBound @(Index dataWidth))
      _ -> False


    dataOut = Just $
      if isJust _last
      then (if insertAtOnce then cache {
              _data = combined
            , _last =  Just $ validIdx + 4
            } else cache {
              _data = combined
            , _last = Nothing
            })
      else cache

    nextSt =
      if readyIn
      then (if insertAtOnce || isNothing _last
            then FcsCopy fwdIn
            else
              FcsInsert {
                  _aborted = _abort
                , _cachedFwd = fwdIn
                , _valid = toEnum $ 3 - fromEnum (maxBound - validIdx)
                , _cachedCrc = leftover
              })
      else currSt

fcsHelperT
  currSt@(FcsInsert{..})
  ( _
  , _
  , PacketStreamS2M readyIn
  )
  = (nextSt, (dataOut, False))
  where
    (dataOut, nextStIfReady) =
      if fromEnum _valid + 1 <=natToNum @dataWidth
      then (
        Just $ PacketStreamM2S
          { _data= fst $ shiftInAt0 (repeat 0) _cachedCrc
          , _last= Just $ resize _valid
          , _meta=(), _abort=_aborted
          }, FcsCopy _cachedFwd)
      else (
        Just $ PacketStreamM2S
          {_data= fst $ shiftInAt0 (repeat 0) _cachedCrc
          , _last= Nothing
          , _meta=()
          , _abort=_aborted}
        , currSt
          {_valid= _valid - natToNum @dataWidth
          , _cachedCrc = fst $ shiftInAtN @dataWidth _cachedCrc (repeat 0)
          })

    nextSt =
      if readyIn
      then nextStIfReady
      else currSt

