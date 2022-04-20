{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

-- GHC is asked to do quite a lot of optimization in the module that calls `inline`,
-- so we're increasing the amount of ticks for the simplifier not to run out of them.
{-# OPTIONS_GHC -fsimpl-tick-factor=200 #-}

module PlutusLedgerApi.Common.Eval where

import PlutusCore
import PlutusCore as ScriptPlutus (Version, defaultVersion)
import PlutusCore.Data as Plutus
import PlutusCore.Evaluation.Machine.CostModelInterface as Plutus
import PlutusCore.Evaluation.Machine.ExBudget as Plutus
import PlutusCore.Evaluation.Machine.ExBudgetingDefaults qualified as Plutus
import PlutusCore.Evaluation.Machine.MachineParameters as Plutus
import PlutusCore.MkPlc qualified as UPLC
import PlutusCore.Pretty
import PlutusLedgerApi.Common.SerialisedScript
import PlutusLedgerApi.Common.Versions
import PlutusPrelude
import UntypedPlutusCore qualified as UPLC
import UntypedPlutusCore.Evaluation.Machine.Cek qualified as UPLC


import Codec.CBOR.Read qualified as CBOR
import Control.Monad.Except
import Control.Monad.Writer
import Data.ByteString.Lazy (fromStrict)
import Data.ByteString.Short
import Data.Text as Text
import Data.Tuple
import GHC.Exts (inline)
import Prettyprinter

-- | Errors that can be thrown when evaluating a Plutus script.
data EvaluationError =
    CekError (UPLC.CekEvaluationException NamedDeBruijn DefaultUni DefaultFun) -- ^ An error from the evaluator itself
    | DeBruijnError FreeVariableError -- ^ An error in the pre-evaluation step of converting from de-Bruijn indices
    | CodecError CBOR.DeserialiseFailure -- ^ A serialisation error
    | IncompatibleVersionError (ScriptPlutus.Version ()) -- ^ An error indicating a version tag that we don't support
    -- TODO: make this error more informative when we have more information about what went wrong
    | CostModelParameterMismatch -- ^ An error indicating that the cost model parameters didn't match what we expected
    deriving stock (Show, Eq)

instance Pretty EvaluationError where
    pretty (CekError e)      = prettyClassicDef e
    pretty (DeBruijnError e) = pretty e
    pretty (CodecError e) = viaShow e
    pretty (IncompatibleVersionError actual) = "This version of the Plutus Core interface does not support the version indicated by the AST:" <+> pretty actual
    pretty CostModelParameterMismatch = "Cost model parameters were not as we expected"

-- | The type of log output: just a list of 'Text'.
type LogOutput = [Text.Text]

-- | A simple toggle indicating whether or not we should produce logs.
data VerboseMode = Verbose | Quiet
    deriving stock (Eq)

-- | Shared helper for the evaluation functions, deserialises the 'SerialisedScript' , applies it to its arguments, puts fakenamedebruijns, and scope-checks it.
mkTermToEvaluate
    :: (MonadError EvaluationError m)
    => LedgerPlutusVersion
    -> ProtocolVersion
    -> SerialisedScript
    -> [Plutus.Data]
    -> m (UPLC.Term UPLC.NamedDeBruijn DefaultUni DefaultFun ())
mkTermToEvaluate lv pv bs args = do
    -- It decodes the program through the optimized ScriptForExecution. See `ScriptForExecution`.
    (_, ScriptForExecution (UPLC.Program _ v t)) <- liftEither $ first CodecError $ CBOR.deserialiseFromBytes (scriptCBORDecoder lv pv) $ fromStrict $ fromShort bs
    unless (v == ScriptPlutus.defaultVersion ()) $ throwError $ IncompatibleVersionError v
    let termArgs = fmap (UPLC.mkConstant ()) args
        appliedT = UPLC.mkIterApp () t termArgs

    -- make sure that term is closed, i.e. well-scoped
    through (liftEither . first DeBruijnError . UPLC.checkScope) appliedT

-- | Which unlifting mode should we use in the given 'ProtocolVersion'
-- so as to correctly construct the machine's parameters
unliftingModeIn :: ProtocolVersion -> UnliftingMode
unliftingModeIn pv =
    -- This just changes once in version 7.0
    if pv >= ProtocolVersion 7 0 then UnliftingDeferred else UnliftingImmediate

type DefaultMachineParameters = MachineParameters CekMachineCosts UPLC.CekValue DefaultUni DefaultFun

toMachineParameters :: ProtocolVersion -> EvaluationContext -> DefaultMachineParameters
toMachineParameters pv = case unliftingModeIn pv of
    UnliftingImmediate -> machineParametersImmediate
    UnliftingDeferred  -> machineParametersDeferred

{- Note [Inlining meanings of builtins]
It's vitally important to inline the 'toBuiltinMeaning' method of a set of built-in functions as
that allows GHC to look under lambdas and completely optimize multiple abstractions away.

There are two ways of doing that: by relying on 'INLINE' pragmas all the way up from the
'ToBuiltinMeaning' instance for the default set of builtins or by ensuring that 'toBuiltinsRuntime'
is compiled efficient by turning it into a one-method class (see
https://github.com/input-output-hk/plutus/pull/4419 for how that looks like). We chose the former,
because it's simpler. Although it's also less reliable: machine parameters are computed in
multiple places and we need to make sure that benchmarking, cost model calculations and the actual
production path have builtins compiled in the same way, 'cause otherwise performance analysis and
cost predictions can be wrong by a big margin without us knowing. Because of this danger in addition
to putting @INLINE@ pragmas on every relevant definition, we also stick a call to 'inline' at the
call site. We also do not attempt to only compile things efficiently where we need that and instead
inline the meanings of builtins everywhere. Just to be sure.

Note that a combination of @INLINABLE@ + 'inline' does not result in proper inlining for whatever
reason. It has to be @INLINE@ (and we add 'inline' on top of that for some additional reliability
as we did have cases where sticking 'inline' on something that already had @INLINE@ would fix
inlining).
-}

mkMachineParametersFor :: (MonadError CostModelApplyError m)
                       => Version ()
                       -> UnliftingMode
                       -> Plutus.CostModelParams
                       -> m DefaultMachineParameters
mkMachineParametersFor ver unlMode newCMP =
    inline Plutus.mkMachineParameters ver unlMode <$>
        Plutus.applyCostModelParams Plutus.defaultCekCostModel newCMP
{-# INLINE mkMachineParametersFor #-}


{-| An opaque type that contains all the static parameters that the evaluator needs to evaluate a
script.  This is so that they can be computed once and cached, rather than recomputed on every
evaluation.

There are two sets of parameters: one is with immediate unlifting and the other one is with
deferred unlifting. We have to keep both of them, because depending on the language version
 either one has to be used or the other. We also compile them separately due to all the inlining
 and optimization that need to happen for things to be efficient.
-}
data EvaluationContext = EvaluationContext
    { machineParametersImmediate :: DefaultMachineParameters
    , machineParametersDeferred  :: DefaultMachineParameters
    }
    deriving stock Generic
    deriving anyclass NFData

{-|  Build the 'EvaluationContext'.

The input is a `Map` of `Text`s to cost integer values (aka `Plutus.CostModelParams`, `Alonzo.CostModel`)
See Note [Inlining meanings of builtins].
-}
mkDynEvaluationContext :: MonadError CostModelApplyError m => Version () -> Plutus.CostModelParams -> m EvaluationContext
mkDynEvaluationContext ver newCMP =
    EvaluationContext
        <$> inline mkMachineParametersFor ver UnliftingImmediate newCMP
        <*> inline mkMachineParametersFor ver UnliftingDeferred newCMP

-- | Comparably expensive to `mkEvaluationContext`, so it should only be used sparingly.
assertWellFormedCostModelParams :: MonadError CostModelApplyError m => Plutus.CostModelParams -> m ()
assertWellFormedCostModelParams = void . Plutus.applyCostModelParams Plutus.defaultCekCostModel

{-|
Evaluates a script, with a cost model and a budget that restricts how many
resources it can use according to the cost model. Also returns the budget that
was actually used.

Can be used to calculate budgets for scripts, but even in this case you must give
a limit to guard against scripts that run for a long time or loop.

Note: Parameterized over the ledger-plutus-version since the builtins allowed (during decoding) differs.
-}
evaluateScriptRestricting
    :: LedgerPlutusVersion
    -> ProtocolVersion
    -> VerboseMode     -- ^ Whether to produce log output
    -> EvaluationContext -- ^ The cost model that should already be synced to the most recent cost-model-params coming from the current protocol
    -> ExBudget        -- ^ The resource budget which must not be exceeded during evaluation
    -> SerialisedScript          -- ^ The script to evaluate
    -> [Plutus.Data]          -- ^ The arguments to the script
    -> (LogOutput, Either EvaluationError ExBudget)
evaluateScriptRestricting lv pv verbose ectx budget p args = swap $ runWriter @LogOutput $ runExceptT $ do
    appliedTerm <- mkTermToEvaluate lv pv p args

    let (res, UPLC.RestrictingSt (ExRestrictingBudget final), logs) =
            UPLC.runCekDeBruijn
                (toMachineParameters pv ectx)
                (UPLC.restricting $ ExRestrictingBudget budget)
                (if verbose == Verbose then UPLC.logEmitter else UPLC.noEmitter)
                appliedTerm

    tell logs
    liftEither $ first CekError $ void res
    pure (budget `minusExBudget` final)

{-|
Evaluates a script, returning the minimum budget that the script would need
to evaluate successfully. This will take as long as the script takes, if you need to
limit the execution time of the script also, you can use 'evaluateScriptRestricting', which
also returns the used budget.

Note: Parameterized over the ledger-plutus-version since the builtins allowed (during decoding) differs.
-}
evaluateScriptCounting
    :: LedgerPlutusVersion
    -> ProtocolVersion
    -> VerboseMode     -- ^ Whether to produce log output
    -> EvaluationContext -- ^ The cost model that should already be synced to the most recent cost-model-params coming from the current protocol
    -> SerialisedScript          -- ^ The script to evaluate
    -> [Plutus.Data]          -- ^ The arguments to the script
    -> (LogOutput, Either EvaluationError ExBudget)
evaluateScriptCounting lv pv verbose ectx p args = swap $ runWriter @LogOutput $ runExceptT $ do
    appliedTerm <- mkTermToEvaluate lv pv p args

    let (res, UPLC.CountingSt final, logs) =
            UPLC.runCekDeBruijn
                (toMachineParameters pv ectx)
                UPLC.counting
                (if verbose == Verbose then UPLC.logEmitter else UPLC.noEmitter)
                appliedTerm

    tell logs
    liftEither $ first CekError $ void res
    pure final
