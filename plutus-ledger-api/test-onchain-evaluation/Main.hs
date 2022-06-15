{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

module Main (main) where

import Plutus.Script.Evaluation.Test.Options qualified as O
import PlutusCore.Evaluation.Machine.CostModelInterface (CostModelApplyError)
import PlutusLedgerApi.Common
import PlutusLedgerApi.Test.EvaluationEvent
import PlutusLedgerApi.V1 qualified as V1
import PlutusLedgerApi.V2 qualified as V2

import Codec.Serialise qualified as CBOR
import Control.Concurrent.ParallelIO.Local qualified as Concurrent
import Control.Exception (evaluate)
import Control.Monad.Extra (whenJust)
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Lazy qualified as Lazy
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty, nonEmpty, toList)
import Data.Maybe
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Traversable (for)
import Options.Applicative qualified as OA
import PlutusCore.Pretty
import Prettyprinter
import PyF (fmt)
import System.Directory.Extra (listFiles)
import System.Exit (die)
import System.FilePath (FilePath, isExtensionOf, (<.>))
import System.IO.Extra (withTempDir)
import System.Process.Extra (system_)

data TestFailure = TestFailure
  { tfEventsFile :: FilePath,
    tfErrors     :: NonEmpty UnexpectedEvaluationResult
  } deriving stock (Show)

instance Pretty TestFailure where
    pretty TestFailure{..} = nest 2 $ vsep
     [ "Test Failures in" <+> pretty tfEventsFile,
       vsep (pretty <$> toList tfErrors)
     ]

main :: IO ()
main = do
    opts <- OA.execParser O.parserInfo
    eventFiles <- filter (O.optsEventFileExt opts `isExtensionOf`) <$> listFiles (O.optsDir opts)
    mbErrs <- for (zip [1 :: Int ..] eventFiles) $ \(i, eventFile) -> do
        putStrLn [fmt|Processing event file {i}: {eventFile}|]
        events <- CBOR.readFileDeserialise @ScriptEvaluationEvents eventFile
        ctxV1 <- either invalidCostParams pure $
           mkContext V1.mkEvaluationContext (eventsCostParamsV1 events)
        ctxV2 <- either invalidCostParams pure $
           mkContext V1.mkEvaluationContext (eventsCostParamsV1 events)
        mbErrs <- Concurrent.withPool (O.optsConcurrency opts) $ \pool -> do
            Concurrent.parallel pool $ fmap (evaluate . runTest ctxV1 ctxV2) (toList (eventsEvents events))
        case nonEmpty (catMaybes mbErrs) of
                Nothing -> do
                    putStrLn "All tests passed"
                    pure Nothing
                Just errs -> do
                    putStrLn [fmt|
Some tests failed. Number of failures: {length errs}
{render . indent 2 . vsep $ pretty <$> toList errs :: Text}
|]
                    pure $ Just (TestFailure eventFile errs)
    whenJust (nonEmpty (catMaybes mbErrs)) $ \errs -> die [fmt|
Total number of failed tests: {sum . fmap length $ tfErrors <$> errs}
Number of event files with failed tests: {length errs}
{render . indent 2 . vsep $ pretty <$> toList errs :: Text}
|]

--                     die
--                         [fmt|
-- Script evaluation regression test failed.
-- Number of failed test cases: {length errs}
-- |]
-- {intercalate "\n" (show <$> errs)}
    where
        runTest :: Maybe EvaluationContext ->
              Maybe EvaluationContext -> ScriptEvaluationEvent -> Maybe UnexpectedEvaluationResult
        runTest ctxV1 ctxV2 event = case event of
            PlutusV1Event{} -> checkEvaluationEvent (fromJust ctxV1) [] event
            PlutusV2Event{} -> checkEvaluationEvent (fromJust ctxV2) [] event

        mkContext f = \case
            Nothing         -> Right Nothing
            Just costParams -> Just <$> f costParams

        invalidCostParams err = die $ "Invalid cost parameters: " <> show err
