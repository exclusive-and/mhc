module MHC.Core (
    -- * MHC Core language encoding
    MtlProgram, MtlBind (..), MtlBndr, MtlExpr (..),
    Alt (..), AltCon (..),

    -- * Var re-exports
    Var,

    -- * Core term constructing functions
    mkApps, mkLams
    ) where

import Data.Foldable

import MHC.Core.Literals
import MHC.Core.Var


-- | An MHC Core program is a list of binds/value definitions.
type MtlProgram = [MtlBind]

{-
********************************************************************
*                                                                  *
    MHC Core language encoding
*                                                                  *
********************************************************************
-}

data MtlBind = NonRec Var  MtlExpr
             | Rec  [(Var, MtlExpr)]

type MtlBndr = Var


data MtlExpr
    = App   MtlExpr MtlExpr
    | Lam   Var MtlExpr
    | Var   Var
    | Let   MtlBind MtlExpr
    | Case  MtlExpr Var MtlType [Alt]


data Alt = Alt AltCon [Var] MtlExpr

data AltCon
    = DataAlt       DataCon     -- ^ Datatype constructor pattern.
    | LitAlt        Literal     -- ^ Literal pattern.
    | DefaultAlt


{-
********************************************************************
*                                                                  *
    Core term constructing functions
*                                                                  *
********************************************************************
-}

mkApps :: MtlExpr -> [MtlExpr] -> MtlExpr
mkApps = foldl' App

mkLams :: [Var] -> MtlExpr -> MtlExpr
mkLams bndrs body = foldr Lam body bndrs


{-
********************************************************************
*                                                                  *
    Data constructors and types
*                                                                  *
********************************************************************
-}

data DataCon = DataCon
    {
        dc_name     :: Var,
        dc_tag      :: ConTag
    }

type ConTag = Int

type MtlType = MtlExpr



