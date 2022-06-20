{- | Conformance tests of Untyped Plutus Core program evaluation. -}

module Main (main) where

import PlutusConformance.Common (TestGroup (..), evalUplcProg, runUplcEvalTests)
import Prelude hiding (readFile)

main :: IO ()
main = runUplcEvalTests [MkTestGroup "CEK machine" evalUplcProg]

