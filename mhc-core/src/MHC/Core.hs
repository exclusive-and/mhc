
module MHC.Core where

import Data.Foldable (foldl')
import MHC.Core.Vars


{-
********************************************************************
*                                                                  *
    MHC MtlCore data encoding
*                                                                  *
********************************************************************
-}

data MtlBind = NonRec Var  MtlExpr
             | Rec  [(Var, MtlExpr)]

type MtlBndr = Var

type MtlProgram = [MtlBind]


data MtlExpr
    = App   MtlExpr MtlExpr
    | Lam   Var MtlExpr
    | Var   Var
    | Let   MtlBind MtlExpr
    | Case  MtlExpr Var MtlType [Alt]


data Alt = AltCon [Var] MtlExpr

data AltCon
    = DataAlt   DataCon     -- ^ Datatype constructor pattern.
    | LitAlt    Literal     -- ^ Literal pattern.
    | Default


{-
********************************************************************
*                                                                  *
    MtlCore term constructing functions
*                                                                  *
********************************************************************
-}

mkApps :: MtlExpr -> [MtlExpr] -> MtlExpr
mkApps = foldl' App

