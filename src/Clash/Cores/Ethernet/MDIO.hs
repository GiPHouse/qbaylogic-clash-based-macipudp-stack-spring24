{-# language NumericUnderscores #-}

module Clash.Cores.Ethernet.MDIO (MDIOResponse (..), MDIORequest (..), mdioComponent) where

import Clash.Prelude
import Data.Maybe ( isJust )

data MDIORequest
  -- | PHY address, REG address
  = MDIORead (Unsigned 5) (Unsigned 5)
  -- | PHY address, REG address, 16-bit data
  | MDIOWrite (Unsigned 5) (Unsigned 5) (BitVector 16)
  deriving (Show, ShowX, Eq, Generic, NFDataX, BitPack)

data MDIOResponse
  = MDIOReadResult (BitVector 16)
  -- ^ contents of register that was read
  | MDIOWriteAck
  deriving (Show, ShowX, Eq, Generic, NFDataX, BitPack)

data State
  = SendPreamble (Index 32) (BitVector 14) (Maybe (BitVector 16))
  -- ^ Send 32 times a 1
  | SendSTOPPARA (Index 14) (BitVector 14) (Maybe (BitVector 16))
  -- ^ Send the STart, OPeration, Physical Address and Register Address
  | SendTAWrite (Index 2) (BitVector 16)
  -- ^ Send 10
  | SendTARead
  -- ^ Wait while receiving Z0 (gives control to other side)
  | WriteD16 (Index 16) (BitVector 16)
  -- ^ Write data to a register
  | ReadD16 (Index 16) (BitVector 16)
  -- ^ Receive data from a register
  | Idle
  -- ^ Waiting for the bridge to issue a new request
  deriving (Show, Eq, Generic, NFDataX)

-- | State transitions in the mealy machine
nextStep :: State -> Maybe MDIORequest -> Bit -> (State, Maybe Bit, Maybe MDIOResponse)
-- Nothing happens
nextStep Idle Nothing _ = (Idle, Nothing, Nothing)
-- Receive a request from the bridge
nextStep Idle (Just req) _   = (SendPreamble 0 stoppara write_data, Nothing, Nothing)
    where
        (stoppara, write_data) = case req of
            (MDIORead phy reg) -> (0b0110 ++# (pack phy) ++# (pack reg), Nothing)
            (MDIOWrite phy reg dat) -> (0b0101 ++# (pack phy) ++# (pack reg), Just dat)

-- Send Preamble
nextStep (SendPreamble cnt stoppara d) _ _ = (nextState, Just 1, Nothing)
  where
    nextState
      | cnt < 31 = SendPreamble (cnt+1) stoppara d
      | otherwise = SendSTOPPARA 0 stoppara d

-- Send all 14 bits that are the same for both read and write requests
-- STart, OPeration, Physical Address, Register Address
nextStep (SendSTOPPARA cnt sendBv dat) _ _ = (nextState, Just out, Nothing)
  where
    -- MSB first!
    out = msb sendBv
    bv' = shiftL sendBv 1
    nextState
      | cnt < 13 = SendSTOPPARA (cnt+1) bv' dat
      | otherwise = case dat of
                        Just d -> SendTAWrite 0 d
                        Nothing -> SendTARead

-- Wait out the turn-around field for Read
-- Calibrated by reading the OUI from the PHY identifier 1 register (address 0x02)
nextStep SendTARead _ _ = (ReadD16 0 0, Nothing, Nothing)

-- Receive data for Read
nextStep (ReadD16 cnt bv) _ inp = (nextState, Nothing, response)
  where
    -- Receive a bit from the register we're reading
    -- Bit 15 first, bit 0 last!
    bv' = replaceBit (0 :: Index 16) inp $ shiftL bv 1
    (response, nextState)
      | cnt >= 15 = (Just $ MDIOReadResult bv', Idle)
      | otherwise = (Nothing, ReadD16 (cnt+1) bv')

-- Send turn-around field for Write
nextStep (SendTAWrite cnt d) _ _ = (nextState, Just out, Nothing)
    where
        (out, nextState)
            | cnt == 0  = (1, SendTAWrite 1 d)
            | otherwise = (0, WriteD16 0 d)

-- Write data to register
nextStep (WriteD16 cnt dat) _ _ = (nextState, Just out, response)
  where
    -- Get a bit from the register write
    -- Bit 15 first, bit 0 last!
    out = msb dat
    bv' = shiftL dat 1
    response
      | cnt == 15 = Just $ MDIOWriteAck
      | otherwise = Nothing
    nextState
      | cnt < 15 = WriteD16 (cnt+1) bv'
      | otherwise = Idle


-- | Calculate how many cycles in `dom` fit into a `MDC` cycle
-- | We're aiming for 1.5Mhz so a single clock cycle is 10^12 / (1.5 * 10^6) = 666_666 picoseconds
-- | It is then divided by 2 because `MDCCycle` only describes half of a clock cycle
-- |
-- | This is slightly slower than the 2.5Mhz maximum but still more than fast enough.
type MDCCycle (dom :: Domain) = DivRU 333_333 (DomainPeriod dom)

{-# NOINLINE mdioComponent #-}
mdioComponent
  :: forall dom period
  . (HiddenClockResetEnable dom, KnownDomain dom, DomainPeriod dom ~ period, KnownNat period, (1 <=? period) ~ 'True)
  -- | eth_mdio in
  => Signal dom Bit
  -- | Receive a request from the bridge
  -> Signal dom (Maybe MDIORequest)
  -- |
  -- 1. eth_mdio out
  -- 2. Output result or ACK when we have it
  -- 3. MDC signal, the clock for MDIO
  -> ( Signal dom (Maybe Bit)
     , Signal dom (Maybe MDIOResponse)
     , Signal dom Bit
     )
mdioComponent mdioIn req = (mdioOut, response', boolToBit <$> mdc)
  where
     -- Artificially run at 1.5Mhz
    mdc = oscillate False (SNat @(MDCCycle dom))
    mdcEn = isFalling False mdc

    (st0, mdioOut, response) = unbundle $ nextStep <$> st1 <*> req <*> mdioIn
    st1 = regEn Idle mdcEn st0

    -- Only output the result or ACK once
    justOnce False v = v
    justOnce _ _ = Nothing
    response' = justOnce <$> (register False $ isJust <$> response) <*> response
