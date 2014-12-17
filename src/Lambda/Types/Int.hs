module Lambda.Types.Int where

import Prelude hiding (succ)
import Data.Set (unions)

import Lambda.Variable
import Lambda.Engine
import Lambda.Parser
import Lambda.Types.Bool

-- Introduction
zero :: Term
zero = Lambda onzero (Lambda onsucc (VarTerm onzero))
       where onzero = Variable "x"
             onsucc = Variable "y"
succ :: Term -> Term
succ n = Lambda onzero (Lambda onsucc
            (Apply
                (Apply
                    (VarTerm onsucc)
                    n
                )
                (Apply
                    (Apply
                        n
                        (VarTerm onzero)
                    )
                    (VarTerm onsucc)
                )
            )
         )
         where onzero = head unusedVars
               onsucc = unusedVars !! 1
               unusedVars = notUsed $ allVar n

-- Elimination
recursion :: Term -> Term -> Term -> Term
recursion n onzero onsucc = Apply (Apply n onzero) onsucc

-- Utils
eqz :: Term -> Term
eqz n = recursion n true (Lambda x (Lambda y false))
        where x = head unusedVars
              y = unusedVars !! 1
              unusedVars = notUsed $ allVar n
pred :: Term -> Term
pred n = recursion n zero (Lambda x (Lambda y (VarTerm x)))
         where x = head unusedVars
               y = unusedVars !! 1
               unusedVars = notUsed $ allVar n
sum :: Term -> Term -> Term
sum a b = Apply sum_a b
          where sum_a = recursion a (Lambda x (VarTerm x))
                            (Lambda n (Lambda res
                                (Lambda x (succ (Apply (VarTerm res) (VarTerm x))))
                            ))
                x = head unusedVars
                n = unusedVars !! 1
                res = unusedVars !! 2
                unusedVars = notUsed $ unions [allVar a, allVar b]

-- Interpretation
showInt :: Term -> String
showInt t =
    let
        onzero = Variable "onzero"
        onsucc = Variable "onsucc"
        term = reduceAll $ applyArgs t $ map VarTerm [onzero, onsucc]
    in
        case explode term of
            (VarTerm x, args) | x == onzero && null args ->
                "zero"
            (VarTerm x, args) | x == onsucc ->
                let
                    n = applyArgs (parseRaw "λx.λy.x") args
                in
                    "succ(" ++ showInt n ++ ")"
            _ -> show term
