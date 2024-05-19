module MHC.Core.Subst where

import Data.HashMap as Map
import MHC.Core


type VarSubst   = HashMap Var CoreExpr
type InScopeSet = HashMap Var Var

data Subst = Subst InScopeSet VarSubst

