{-|
A collection of Hedgehog helpers to test Circuit components. To test a
protocol component against a combinatorial model, see 'idWithModel'. To write
your own tester, see 'Test'.
-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language NamedFieldPuns #-}
{-# language UndecidableInstances #-}

module Test.Cores.Ethernet.MaybeControl (
  propWithModelMaybeControl
  , propWithModelMaybeControlSingleDomain
) where


import Clash.Cores.Ethernet.PacketStream

-- base
import Data.Proxy ( Proxy(Proxy) )
import GHC.Stack ( HasCallStack )
import Prelude

-- clash-protocols
import Protocols
import Protocols.Hedgehog.Internal

-- clash-prelude
import Clash.Prelude qualified as C

-- hedgehog
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Property qualified as H
import Hedgehog.Range qualified as Range

-- pretty print
import Data.Maybe
import Text.Show.Pretty ( ppShow )


-- | Whether to stall or not. Used in 'idWithModel'.
data StallMode = NoStall | Stall
  deriving (Show, Enum, Bounded)

-- | Like 'C.resetGenN', but works on 'Int' instead of 'C.SNat'. Not
-- synthesizable.
resetGen :: C.KnownDomain dom => Int -> C.Reset dom
resetGen n = C.unsafeFromHighPolarity
  (C.fromList (replicate n True <> repeat False))

-- | Like propWithModel
-- returns a list of maybe (Packetstream dom dataWidth metaType) instead of (PacketStream dom dataWidth metaType)
propWithModelMaybeControl ::
  forall (dom :: C.Domain) (dataWidth :: C.Nat) (metaType :: C.Type)  .
  (C.KnownDomain dom) =>
  (Test (PacketStream dom dataWidth metaType), HasCallStack) =>
  -- | Options, see 'ExpectOptions'
  ExpectOptions ->
  -- | Test data generator
  H.Gen [PacketStreamM2S dataWidth metaType] ->
  -- | Model
  ([PacketStreamM2S dataWidth metaType] -> [Maybe (PacketStreamM2S dataWidth metaType)]) ->
  -- | Implementation
  Circuit (PacketStream dom dataWidth metaType) (PacketStream dom dataWidth metaType)  ->
  -- | Property to test for. Function is given the data produced by the model
  -- as (Maybe (PacketStream dom dataWidth metaType)) first argument, and the sampled data as a second argument.
  ([Maybe (PacketStreamM2S dataWidth metaType)]  -> [Maybe (PacketStreamM2S dataWidth metaType)]  -> H.PropertyT IO ()) ->
  H.Property
propWithModelMaybeControl eOpts genData model prot prop = H.property $ do
  dat <- H.forAll genData
  let n = maximum (expectToLengths (Proxy @(PacketStream dom dataWidth metaType)) dat)

  let genStall = Gen.integral (Range.linear 0 10)


  -- Generate stalls for LHS part of the protocol. The first line determines
  -- whether to stall or not. The second determines how many cycles to stall
  -- on each _valid_ cycle.
  lhsStallModes <- H.forAll (sequenceA (C.repeat @1 genStallMode))
  lhsStalls <- H.forAll (traverse (genStalls genStall n) lhsStallModes)

  -- Generate stalls for RHS part of the protocol. The first line determines
  -- whether to stall or not. The second determines how many cycles to stall
  -- on each _valid_ cycle.
  rhsStallModes <- H.forAll (sequenceA (C.repeat @1 genStallMode))
  rhsStalls <- H.forAll (traverse (genStalls genStall n) rhsStallModes)

  let
    simConfig = def {resetCycles = eoResetCycles eOpts}
    simDriveConfig =
      if eoDriveEarly eOpts
      then def {resetCycles = max 1 (eoResetCycles eOpts - 5)}
      else def {resetCycles = eoResetCycles eOpts}
    expected = model dat
    lhsStallC = stallC simConfig lhsStalls
    rhsStallC = stallC simConfig rhsStalls
    drivenProtocol =
         driveC simDriveConfig (Just <$> dat)
          |> lhsStallC
          |> prot
          |> rhsStallC
    sampled = sampleC simConfig drivenProtocol
    lengths = pure $ length expected

  -- testSpecificExpectN errors if circuit does not produce enough data
  trimmed <- testSpecificExpectN eOpts lengths sampled

  _ <- H.evalNF trimmed
  _ <- H.evalNF expected

  prop expected trimmed

  where
-- | ExpectN for a model that needs control over the value of maybes
    testSpecificExpectN ::
      forall m .
      (HasCallStack, H.MonadTest m) =>
      -- | Timeout
      ExpectOptions ->
      -- | Expected number of values
      C.Vec 1 Int ->
      -- | Sampled data
      [Maybe (PacketStreamM2S dataWidth metaType)] ->
      -- | packaged results
      m [Maybe (PacketStreamM2S dataWidth metaType)]

    testSpecificExpectN (ExpectOptions{eoEmptyTail, eoTimeout}) (C.head -> nExpected) sampled = do
      go (fromMaybe maxBound eoTimeout) nExpected 0 sampled
        where
        go ::
          HasCallStack =>
          -- Timeout counter. If it reaches zero we time out.
          Int ->
          -- Expected number of values
          Int ->
          -- Amount of Nothings encountered
          Int ->
          -- Sampled data
          [Maybe (PacketStreamM2S dataWidth metaType)] ->
          -- Results
          m [Maybe (PacketStreamM2S dataWidth metaType)]

        go _timeout _n _nothingAm []  =
          -- This really should not happen, protocols should produce data indefinitely
          error "unexpected end of signal"
        go _timeout 0 nothingAm rest = do
          -- Check for superfluous output from protocol
          case catMaybes (take eoEmptyTail rest) of
            [] -> pure (take (nExpected + nothingAm) sampled)
            superfluous ->
              let err = "Circuit produced more output than expected:" in
              H.failWith Nothing (err <> "\n\n" <> ppShow superfluous)
        go timeout n nothingAm _rest | timeout <= 0 =
          H.failWith Nothing $ concat
            [ "Circuit did not produce enough output. Expected "
            , show n, " more values. Sampled only " <> show (nExpected + nothingAm - n) <> ":\n\n"
            , ppShow $ catMaybes (take (nExpected + nothingAm - n) sampled)]

        go timeout n nothingAm (Nothing:as) = do
          -- Circuit did not output valid cycle, increment nothingAmount and continue
          go (pred timeout) n (succ nothingAm) as
        go _ n nothingAm (Just _:as) =
          -- Circuit produced a valid cycle, reset timeOut
          go (fromMaybe maxBound eoTimeout) (pred n) nothingAm as

    -- | Generator for 'StallMode'. Shrinks towards 'NoStall'.
    genStallMode :: H.Gen StallMode
    genStallMode = Gen.enumBounded

    -- | Generator for 'StallMode'. Shrinks towards 'StallWithNack'.
    genStallAck :: H.Gen StallAck
    genStallAck = Gen.enumBounded

    -- | Generator for stall information for 'stallC'. Generates stalls according
    -- to distribution given in first argument. The second argument indicates how
    -- many cycles the component is expecting / is producing data for. If the last
    -- argument is 'NoStall', no stalls will be generated at all.
    genStalls :: H.Gen Int -> Int -> StallMode -> H.Gen (StallAck, [Int])
    genStalls genInt n = \case
      NoStall -> (,[]) <$> genStallAck
      Stall -> (,) <$> genStallAck <*> Gen.list (Range.singleton n) genInt


propWithModelMaybeControlSingleDomain ::
  forall dom (dataWidth :: C.Nat ) (metaType :: C.Type) .
  (Test (PacketStream dom dataWidth metaType), C.KnownDomain dom, HasCallStack) =>
  -- | Options, see 'ExpectOptions'
  ExpectOptions ->
  -- | Test data generator
  H.Gen [PacketStreamM2S dataWidth metaType] ->
  -- | Model
  (C.Clock dom -> C.Reset dom -> C.Enable dom -> ([PacketStreamM2S dataWidth metaType] -> [Maybe (PacketStreamM2S dataWidth metaType)])) ->
  -- | Implementation
  (C.Clock dom -> C.Reset dom -> C.Enable dom -> Circuit (PacketStream dom dataWidth metaType) (PacketStream dom dataWidth metaType)) ->
  -- | Property to test for. Function is given the data produced by the model
  -- as a first argument, and the sampled data as a second argument.
    ([Maybe (PacketStreamM2S dataWidth metaType)]  -> [Maybe (PacketStreamM2S dataWidth metaType)]  -> H.PropertyT IO ()) ->
  H.Property

propWithModelMaybeControlSingleDomain eOpts genData model0 circuit0 =
  propWithModelMaybeControl eOpts genData model1 circuit1
 where
  clk = C.clockGen
  rst = resetGen (eoResetCycles eOpts)
  ena = C.enableGen

  model1 = model0 clk rst ena
  circuit1 = circuit0 clk rst ena
