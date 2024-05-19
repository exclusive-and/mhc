module MHC.Core.Simplify where

import Data.HashMap as Map
import MHC.Core
import MHC.Core.Subst


{-
Here is where the simplification algorithm lives.

Note [Introducing the simplifier]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
At this point in compilation, Core programs are highly likely to contain a lot
of operations that can easily be eliminated at compile-time. The simplifier
searches a program for these operations, and applies some basic rewriting rules
to get rid of them.

The gist of the algorithm is that it gradually disassembles an expression, then
rebuilds it with simplifications applied. Disassembling the expression allows
it to gather all of the elimination-like operations into the SimplCont stack.
And then rebuilding mainly consists of applying any of the collected eliminations
that are allowed by the Core calculus.

The elimination-like operations that the simplifier's interested in are e.g.

      * App, which eliminates Lam by beta-reduction.

      * Case, which eliminates constructors and thunks.

        Eliminating via Case is only possible when other simplifications have already
        reduced the scrutinee of the operation to WHNF.
-}


data SimplEnv = SimplEnv {
        simpl_inScope   :: InScopeSet
    }

data SimplCont
    = Stop
    | ApplyToVal MtlExpr SimplEnv SimplCont


{-
********************************************************************
*                                                                  *
    MHC Core expression simplification
*                                                                  *
********************************************************************
-}

simplExpr   :: SimplEnv
            -> MtlExpr          -- ^ Core term to simplify
            -> SimplCont        -- ^ Deferred work
            -> SimplM MtlExpr

simplExpr env (Var v) cont = simplVar env v cont

simplExpr env (App fun arg) cont
    = simplExpr env fun $
        ApplyToVal arg env cont

simplExpr env (Lam bndr body) (ApplyToVal arg argEnv cont)
    = simplNonRec env bndr (arg, argEnv) body cont

simplExpr env (Let (NonRec bndr rhs) body) cont
    = simplNonRec env bndr (rhs, env) body cont


{-
********************************************************************
*                                                                  *
    Simplifying variables
*                                                                  *
********************************************************************
-}

simplVar :: SimplEnv -> MtlBndr -> SimplM MtlExpr

-- simplVar decides whether to inline, but does /NOT/ do alpha-renaming!
-- The renaming step is kicked downstream to simplNonRec & friends.

simplVar (SimplEnv { simpl_inScope = inScope }) v
    = case Map.lookup inScope v of
        Nothing     -> error "tried looking up a variable that wasn't in scope"
        Just v'     -> Var v'


{-
********************************************************************
*                                                                  *
    Simplifying bindings in expressions
*                                                                  *
********************************************************************
-}

simplNonRec :: SimplEnv
            -> MtlBndr
            -> (MtlExpr, SimplEnv)
            -> MtlExpr
            -> SimplCont
            -> SimplM MtlExpr

-- simplNonRec decides on alpha-renamings.

simplNonRec env bndr (rhs, rhsEnv) body cont = do
    (env1, bndr1) <- simplNonRecBndr env bndr
    env3 <- simplLazyBind (bndr, env) (bndr1, env1) (rhs, rhsEnv)
    expr1 <- simplNonRecBody env3 body cont
    return expr1


simplNonRecBndr :: SimplEnv
                -> MtlBndr
                -> SimplM (SimplEnv, MtlBndr)

simplNonRecBndr env@(SimplEnv { simpl_inScope = inScope, simpl_subst }) oldId
    = ( env { simpl_inScope   = newInScope,
              simpl_subst     = newSubst },
        newId )
    where
    newInScope = Map.insert oldId newId inScope


simplLazyBind   :: (MtlBndr, SimplEnv)
                -> (MtlBndr, SimplEnv)
                -> (MtlExpr, SimplEnv)
                -> SimplM SimplEnv

simplLazyBind (bndr, unfoldEnv) (bndr1, env) (bndr, rhsEnv) = do
    undefined


simplNonRecBody :: SimplEnv
                -> MtlExpr
                -> SimplCont
                -> SimplM MtlExpr

simplNonRecBody = simplExpr
