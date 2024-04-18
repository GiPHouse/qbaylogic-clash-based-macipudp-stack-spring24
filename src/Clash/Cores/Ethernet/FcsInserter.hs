{-# language FlexibleContexts #-}
{-# language MultiParamTypeClasses #-}
{-# language RecordWildCards #-}
{-# language ScopedTypeVariables #-}

module Clash.Cores.Ethernet.FcsInserter (
  fcsInserter
  , fcsInserterC
) where
import Clash.Cores.Crc
import Clash.Cores.Crc.Catalog

import Clash.Cores.Ethernet.PacketStream
import Clash.Cores.Ethernet.Util
import Clash.Prelude
import Data.Maybe
import Protocols
import Data.Data (Proxy(Proxy))


fragment2bv
    :: PacketStreamM2S 4 () -> (Index 4, Vec 4 (BitVector 8))
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

data FcsInserterState
  = FcsCopy
      { _cachedFwd :: Maybe (PacketStreamM2S 4 ()) }
  | FcsInsert
      { _cachedFwd :: Maybe (PacketStreamM2S 4 ())
      , _bytesRemaining :: Index 4
      , _crc :: Vec 4 (BitVector 8)
      }
  deriving (Show, Generic, NFDataX)


fcsInserter
  :: forall (dom :: Domain)
  .  HiddenClockResetEnable dom
  => HardwareCrc Crc32_ethernet 8 4
  => ( Signal dom (Maybe (PacketStreamM2S 4 ()))
     , Signal dom PacketStreamS2M
     )
  -> ( Signal dom PacketStreamS2M
     , Signal dom (Maybe (PacketStreamM2S 4 ()))
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


fcsInserterC
  :: forall (dom :: Domain)
  .  KnownDomain dom
  => HiddenClockResetEnable dom
  => HardwareCrc Crc32_ethernet 8 4
  => Circuit
    (PacketStream dom 4 ())
    (PacketStream dom 4 ())
fcsInserterC = forceResetSanity |> fromSignals fcsInserter

fcsHelper
  :: HiddenClockResetEnable dom
  => ( Signal dom (Vec 4 (BitVector 8))
     , Signal dom (Maybe (PacketStreamM2S 4 ()))
     , Signal dom PacketStreamS2M)
  -> ( Signal dom (Maybe (PacketStreamM2S 4 ()))
     , Signal dom Bool
     )
fcsHelper = mealyB fcsHelperT (FcsCopy Nothing)

fcsHelperT
  :: FcsInserterState
  -> ( Vec 4 (BitVector 8)
     , Maybe (PacketStreamM2S 4 ())
     , PacketStreamS2M)
  -> ( FcsInserterState
     , ( Maybe (PacketStreamM2S 4 ())
       , Bool
       ))
fcsHelperT
  (FcsCopy Nothing)
  ( _
  , fwdIn
  , _
  )
  = (FcsCopy fwdIn, (Nothing, True))

fcsHelperT
  currSt@(FcsCopy (Just cache@(PacketStreamM2S{..})))
  ( ethCrcBytes
  , fwdIn
  , PacketStreamS2M readyIn
  )
  = (nextSt, (dataOut, readyOut))
  where
    validIdx = fromMaybe 3 _last
    (combined, leftover) = splitAt d4 $ appendVec (fromJust _last) _data ethCrcBytes

    dataOut = Just $
      if isJust _last
      then cache {
          _data = combined
        , _last = Nothing
        }
      else cache

    readyOut = readyIn

    nextSt = case (readyOut, _last) of
      (False, _) -> currSt
      (True, Just _) ->
        FcsInsert {
            _cachedFwd = fwdIn 
          , _bytesRemaining = validIdx
          , _crc = leftover
         }
      (True, Nothing) -> FcsCopy fwdIn


fcsHelperT
  currSt@(FcsInsert{..})
  ( _
  , _
  , PacketStreamS2M readyIn
  )
  = (nextSt, (dataOut, False))
  where
    dataOut =
      Just PacketStreamM2S {
        _data=_crc
      , _last=Just _bytesRemaining
      , _meta=()
      , _abort = False
      }
    nextSt = if readyIn
             then FcsCopy _cachedFwd
             else currSt

-- manual testing. Should be replaced with functions defined in Test.Cores.Ethernet.PacketBuffer at some point.

-- charToBv :: Char -> Integer
-- charToBv = fromIntegral . ord

-- chopBy :: Int -> [a] -> [[a]]
-- chopBy _ [] = []
-- chopBy n xs = as : chopBy n bs where (as,bs) = List.splitAt n xs


-- fwdIn :: [Maybe (PacketStreamM2S 4 ())]
-- fwdIn = [ Nothing,
--     Just (PacketStreamM2S (unsafeFromList $ [0,0,0,0]) Nothing () False)
--   , Just (PacketStreamM2S (unsafeFromList $ [1,0,0,0]) (Just 0) () False)]
--   List.++ [ Just (PacketStreamM2S (unsafeFromList $ [i,0,0,0]) Nothing () False) | i <- [2..10]]
--   List.++ [ Just (PacketStreamM2S (unsafeFromList $ [11,0,0,0]) (Just 3) () False)]
--   List.++ [ Just (PacketStreamM2S (unsafeFromList $ [i,0,0,0]) Nothing () False) | i <- [12..15]]
--   List.++ [ Just (PacketStreamM2S (unsafeFromList $ [0,1,0,0]) (Just 2) () False)]

-- deriveHardwareCrc (Proxy @Crc32_ethernet) d8 d4
-- fcsInserter' = exposeClockResetEnable @System fcsInserter systemClockGen resetGen enableGen
-- fcsOut = fcsInserter' (fromList fwdIn, pure $ PacketStreamS2M True)
-- fcsFromHardware = sampleN 10000 $ snd fcsOut

-- crcEngine' = exposeClockResetEnable crcEngine systemClockGen resetGen enableGen
-- myEngine = crcEngine' $ Proxy @Crc32_ethernet


-- packets = List.groupBy (\x _ -> isJust x && (isJust . _last . fromJustX $ x)) fwdIn
-- expectedOut = M.join [sampleN (List.length packet + 1) $ myEngine (fromList $ True : List.repeat False) (fromList $ fmap fragment2bv <$> packet) | packet <- packets]


