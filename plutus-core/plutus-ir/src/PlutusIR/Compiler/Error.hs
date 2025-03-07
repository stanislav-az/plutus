{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
module PlutusIR.Compiler.Error (Error (..), AsError (..)) where

import PlutusCore qualified as PLC
import PlutusCore.Pretty qualified as PLC

import Control.Exception
import Control.Lens

import Data.Text qualified as T
import Data.Typeable
import Prettyprinter ((<+>))
import Prettyprinter qualified as PP

data Error uni fun a = CompilationError a T.Text -- ^ A generic compilation error.
                     | UnsupportedError a T.Text -- ^ An error relating specifically to an unsupported feature.
                     | PLCError (PLC.Error uni fun a) -- ^ An error from running some PLC function, lifted into this error type for convenience.
makeClassyPrisms ''Error

instance PLC.AsTypeError (Error uni fun a) (PLC.Term PLC.TyName PLC.Name uni fun ()) uni fun a where
    _TypeError = _PLCError . PLC._TypeError

instance (PP.Pretty (PLC.SomeTypeIn uni), PLC.Closed uni, uni `PLC.Everywhere` PLC.PrettyConst, PP.Pretty fun, PP.Pretty a) =>
         Show (Error uni fun a) where
    show = show . PLC.prettyPlcClassicDebug

instance (PP.Pretty (PLC.SomeTypeIn uni), PLC.Closed uni, uni `PLC.Everywhere` PLC.PrettyConst, PP.Pretty fun, PP.Pretty a) =>
            PLC.PrettyBy PLC.PrettyConfigPlc (Error uni fun a) where
    prettyBy config = \case
        CompilationError x e -> "Error during compilation:" <+> PP.pretty e <> "(" <> PP.pretty x <> ")"
        UnsupportedError x e -> "Unsupported construct:" <+> PP.pretty e <+> "(" <> PP.pretty x <> ")"
        PLCError e           -> PP.vsep [ "Error from the PLC compiler:", PLC.prettyBy config e ]

deriving anyclass instance
    ( PP.Pretty (PLC.SomeTypeIn uni), PLC.Closed uni, uni `PLC.Everywhere` PLC.PrettyConst, PP.Pretty a, PP.Pretty fun
    , Typeable uni, Typeable fun, Typeable a
    ) => Exception (Error uni fun a)
