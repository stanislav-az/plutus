module UntypedPlutusCore (
    module Export
    , Term (..)
    , Program (..)
    , applyProgram
    , parseScoped
    , PLC.DefaultUni
    , PLC.DefaultFun
    , mkProg
    , mkTerm
    ) where

import UntypedPlutusCore.Check.Scope as Export
import UntypedPlutusCore.Core as Export
import UntypedPlutusCore.DeBruijn as Export
import UntypedPlutusCore.Parser as Parser (parseScoped)
import UntypedPlutusCore.Simplify as Export
import UntypedPlutusCore.Size as Export
import UntypedPlutusCore.Subst as Export

import PlutusCore.Core qualified as PLC
import PlutusCore.Default qualified as PLC
import PlutusCore.Name as Export

-- | Take one UPLC program and apply it to another.
applyProgram :: Program name uni fun () -> Program name uni fun () -> Program name uni fun ()
applyProgram (Program _ _ t1) (Program _ _ t2) = Program () (PLC.defaultVersion ()) (Apply () t1 t2)

-- | Turn a UPLC term to a UPLC program.
mkProg :: Term name uni fun () -> Program name uni fun ()
mkProg = Program () (PLC.defaultVersion ())

-- | Turn a UPLC program to a UPLC term.
mkTerm :: Program name uni fun () -> Term name uni fun ()
mkTerm (Program _ _ t) = t
