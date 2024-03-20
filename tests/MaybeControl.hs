{-|
A collection of Hedgehog helpers to test Circuit components. To test a
protocol component against a combinatorial model, see 'idWithModel'. To write
your own tester, see 'Test'.
-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

import Clash.Cores.Ethernet.PacketStream

-- base
import Prelude
import GHC.Stack (HasCallStack)
import Data.Proxy (Proxy(Proxy))

-- clash-protocols
import Protocols
import Protocols.Hedgehog.Internal

-- clash-prelude
import qualified Clash.Prelude as C

-- hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Internal.Property as H
import Data.Maybe

-- | Test a protocol against a pure model implementation. Circuit under test will
-- be arbitrarily stalled on the left hand and right hand side and tested for
-- a number of properties:
--
--   * Whether it does not produce too little data.
--   * Whether it does not produce /more/ data than expected.
--   * Whether it responds to backpressure correctly
--   * Whether it (eventually) drives a /nack/ while in reset.
--
-- The property samples the length of the data produced by the model as a maximum.
-- Finally, the data will be tested against the property supplied in the last argument.
--
propWithModelMaybeControl ::
  forall (dataWidth :: C.Nat) (metaType :: C.Type) (dom :: C.Domain) .
  (C.KnownDomain dom) =>
  (Test (PacketStream dom dataWidth metaType), HasCallStack) =>
  -- | Options, see 'ExpectOptions'
  ExpectOptions ->
  -- | Test data generator
  H.Gen [Maybe (PacketStreamM2S dataWidth metaType)] ->
  -- | Model 
  ([Maybe (PacketStreamM2S dataWidth metaType)] -> [Maybe (PacketStreamM2S dataWidth metaType)]) ->
  -- | Implementation
  Circuit (PacketStream dom dataWidth metaType) (PacketStream dom dataWidth metaType)  ->
  -- | Property to test for. Function is given the data produced by the model
  -- as (PacketStream dom dataWidth metaType) first argument, and the sampled data as a second argument.
  ([Maybe (PacketStreamM2S dataWidth metaType)]  -> [Maybe (PacketStreamM2S dataWidth metaType)]  -> H.PropertyT IO ()) ->
  H.Property
propWithModelMaybeControl eOpts genData model prot prop = H.property $ do
  dat <- H.forAll genData

  let
    simConfig = def {resetCycles = eoResetCycles eOpts}
    simDriveConfig =
      if eoDriveEarly eOpts
      then def {resetCycles = max 1 (eoResetCycles eOpts - 5)}
      else def {resetCycles = eoResetCycles eOpts}
    expected = model dat
    drivenProtocol =
         driveC simDriveConfig dat
      |> prot
    sampled = sampleC simConfig drivenProtocol
    lengths = pure $ length expected

  -- testSpecificExpectN errors if circuit does not produce enough data
  trimmed <- testSpecificExpectNz (eoTimeout eOpts) lengths sampled

  _ <- H.evalNF trimmed
  _ <- H.evalNF expected

  prop expected trimmed

-- | ExpectN for a model that needs control over the value of maybes
testSpecificExpectN ::
  forall m (dataWidth :: C.Nat) (metaType :: C.Type).
  (HasCallStack, H.MonadTest m) =>
  -- | Timeout
  Maybe Int -> 
  -- | Expected number of values  
  C.Vec 1 Int -> 
  -- | Sampled data
  [Maybe (PacketStreamM2S dataWidth metaType)] -> 
  -- | packaged results
  m [Maybe (PacketStreamM2S dataWidth metaType)]
testSpecificExpectN timeout (C.head -> nExpected) sampled = do
  go (fromMaybe maxBound timeout) nExpected sampled
  where
    go ::
      HasCallStack =>
      -- Timeout counter. If it reaches zero we time out.
      Int ->
      -- Expected number of values
      Int ->
      -- Sampled data
      [Maybe (PacketStreamM2S dataWidth metaType)] ->
      -- Results
      m [Maybe (PacketStreamM2S dataWidth metaType)]
    go _timeout _n [] =
      -- This really should not happen, protocols should produce data indefinitely
      error "unexpected end of signal"
    go _timeout 0 rest = do
      -- Check for superfluous output from protocol
      case catMaybes (take eoEmptyTail rest) of
        [] -> pure (take nExpected (catMaybes sampled))
        superfluous ->
          let err = "Circuit produced more output than expected:" in
          H.failWith Nothing (err <> "\n\n" <> ppShow superfluous)
    go _timeout n _ | _timeout <= 0 =
      H.failWith Nothing $ concat
        [ "Circuit did not produce enough output. Expected "
        , show n, " more values. Sampled only " <> show (nExpected - n) <> ":\n\n"
        , ppShow (take (nExpected - n) (catMaybes sampled)) ]

    go _timeout n (Nothing:as) = do
      -- Circuit did not output valid cycle, just continue
      go (pred _timeout) n as
    go _ n (Just _:as) =
      -- Circuit produced a valid cycle, reset timeout
      go (fromMaybe maxBound timeout) (pred n) as
