{-# language RecordWildCards #-}

module Clash.Cores.Ethernet.DownConverter
  ( downConverter
  , downConverterC
  ) where

import Clash.Cores.Ethernet.PacketStream
import Clash.Prelude
import Data.Maybe ( isJust )
import Protocols ( Circuit, fromSignals, (|>) )
import Clash.Cores.Ethernet.Util ( toMaybe )

data DownConverterState (dataWidth :: Nat) =
  DownConverterState {
    _dcBuf :: Vec dataWidth (BitVector 8),
    -- ^ Buffer
    _dcSize :: Index (dataWidth + 1),
    -- ^ Number of valid bytes in _dcBuf
    _dcLastVec :: Bool,
    -- ^ True if last byte of _dcBuf was marked as last byte by incoming stream
    _dcAborted :: Bool
    -- ^ If True, outgoing bytes should be marked as aborted until _dcBuf is replaced
  }
  deriving (Generic, NFDataX)

-- | Computes new state from incoming data
fromPacketStreamM2S
  :: forall (dataWidth :: Nat) .
  KnownNat dataWidth
  => PacketStreamM2S dataWidth ()
  -> DownConverterState dataWidth
fromPacketStreamM2S (PacketStreamM2S vs lastIdx _ aborted) =
  DownConverterState
    { _dcBuf = vs
    , _dcSize = maybe (natToNum @dataWidth) (succ . resize) lastIdx -- lastIdx points to the last valid byte, so the buffer size is one more
    , _dcLastVec = isJust lastIdx
    , _dcAborted = aborted
    }

-- | Computes output of down converter
toMaybePacketStreamM2S
  :: forall (dataWidth :: Nat) .
  1 <= dataWidth
  => KnownNat dataWidth
  => DownConverterState dataWidth
  -> Maybe (PacketStreamM2S 1 ())
toMaybePacketStreamM2S DownConverterState {..} = toMaybe (_dcSize > 0) out
  where
    out = PacketStreamM2S
      { _data = leToPlusKN @1 @dataWidth head _dcBuf :> Nil
      , _last = toMaybe (_dcSize == 1 && _dcLastVec) 0
      , _meta = ()
      , _abort = _dcAborted
      }

downConverter
  :: forall (dataWidth :: Nat) (dom :: Domain).
  HiddenClockResetEnable dom
  => 1 <= dataWidth
  => KnownNat dataWidth
  => ( Signal dom (Maybe (PacketStreamM2S dataWidth ()))
     , Signal dom PacketStreamS2M
     )
  -- ^ Input packet stream from the source and backpressure from the sink
  -> ( Signal dom PacketStreamS2M
     , Signal dom (Maybe (PacketStreamM2S 1 ()))
     )
  -- ^ Output backpressure to the source
  --   Output packet stream to the sink
downConverter = mealyB go s0
  where
    s0 = DownConverterState
      { _dcBuf = errorX "downConverter: undefined initial value"
      , _dcSize = 0
      , _dcLastVec = False
      , _dcAborted = False
      }
    go
      :: DownConverterState dataWidth
      -> (Maybe (PacketStreamM2S dataWidth ()), PacketStreamS2M)
      -> (DownConverterState dataWidth, (PacketStreamS2M, Maybe (PacketStreamM2S 1 ())))
    go st@(DownConverterState {..}) (Nothing, PacketStreamS2M inReady) = (st', (PacketStreamS2M outReady, toMaybePacketStreamM2S st))
      where
        (_dcSize',_dcBuf') = if _dcSize > 0 && inReady 
                   then (_dcSize - 1, _dcBuf <<+ 0)
                   else (_dcSize, _dcBuf)

        -- If the next buffer contains no valid bytes,
        -- and the final byte was acknowledged, we can 
        -- acknowledge the newly received data.
        -- The || is lazy, and we need this: if the output
        -- of the downconverter is Nothing, we are not allowed to
        -- evaluate inReady.
        outReady = _dcSize == 0 || (_dcSize' == 0 && inReady)
        st' = st 
                { _dcBuf = _dcBuf'
                , _dcSize = _dcSize'
                }

    go st@(DownConverterState {..}) (Just packetStream, PacketStreamS2M inReady) = (st', (PacketStreamS2M outReady, toMaybePacketStreamM2S st))
      where
        (_dcSize',_dcBuf') = if _dcSize > 0 && inReady 
                   then (_dcSize - 1, _dcBuf <<+ 0)
                   else (_dcSize, _dcBuf)
     
        outReady = _dcSize == 0 || (_dcSize' == 0 && inReady)

        st'
          | outReady = fromPacketStreamM2S packetStream
          | otherwise = st 
                          { _dcBuf = _dcBuf'
                          , _dcSize = _dcSize'
                          }


downConverterC
  :: forall (dataWidth :: Nat) (dom :: Domain).
  HiddenClockResetEnable dom
  => 1 <= dataWidth
  => KnownNat dataWidth
  => Circuit (PacketStream dom dataWidth ()) (PacketStream dom 1 ())
downConverterC = forceResetSanity |> fromSignals downConverter
