{-# LANGUAGE DeriveAnyClass #-}

module PlutusLedgerApi.Test.EvaluationEvent (
    ScriptEvaluationEvents (..),
    ScriptEvaluationEvent (..),
    ScriptEvaluationData (..),
    ScriptEvaluationResult (..),
) where

import Plutus.ApiCommon
import PlutusCore.Data qualified as PLC
import PlutusCore.Evaluation.Machine.ExBudget (ExBudget)

import Codec.Serialise (Serialise (..))
import Data.List.NonEmpty (NonEmpty (..))
import GHC.Generics (Generic)

data ScriptEvaluationResult = ScriptEvaluationSuccess | ScriptEvaluationFailure
    deriving stock (Generic)
    deriving anyclass (Serialise)

-- | All the data needed to evaluate a script using the ledger API, except for the cost model
-- parameters, as these are tracked separately.
data ScriptEvaluationData = ScriptEvaluationData
    { dataProtocolVersion :: ProtocolVersion
    , dataBudget          :: ExBudget
    , dataScript          :: SerializedScript
    , dataInputs          :: [PLC.Data]
    }
    deriving stock (Generic)
    deriving anyclass (Serialise)

-- | Information about an on-chain script evaluation event, specifically the information needed
-- to evaluate the script, and the expected result.
data ScriptEvaluationEvent
    = PlutusV1Event ScriptEvaluationData ScriptEvaluationResult
    | PlutusV2Event ScriptEvaluationData ScriptEvaluationResult
    deriving stock (Generic)
    deriving anyclass (Serialise)

-- | This type contains a list of on-chain script evaluation events. All PlutusV1
-- evaluations (if any) share the same cost parameters. Same with PlutusV2.
--
-- Sharing the cost parameters lets us avoid creating a new `EvaluationContext` for
-- each `ScriptEvaluationEvent`.
data ScriptEvaluationEvents = ScriptEvaluationEvents
    { eventsCostParamsV1 :: Maybe [Integer]
    -- ^ Cost parameters shared by all PlutusV1 evaluation events in `eventsEvents`, if any.
    , eventsCostParamsV2 :: Maybe [Integer]
    -- ^ Cost parameters shared by all PlutusV2 evaluation events in `eventsEvents`, if any.
    , eventsEvents       :: NonEmpty ScriptEvaluationEvent
    }
    deriving stock (Generic)
    deriving anyclass (Serialise)
