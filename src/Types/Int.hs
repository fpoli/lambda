module Types.Int where

import Prelude hiding (succ)
import Data.Set (unions)
import Variable
import Lambda
import Parser
import Types.Bool

-- Introduzione
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

-- Eliminazione
rec :: Term -> Term -> Term -> Term
rec n onzero onsucc = Apply (Apply n onzero) onsucc

-- Utilità
eqz :: Term -> Term
eqz n = rec n true (Lambda x (Lambda y false))
        where x = head unusedVars
              y = unusedVars !! 1
              unusedVars = notUsed $ allVar n
pred :: Term -> Term
pred n = rec n zero (Lambda x (Lambda y (VarTerm x)))
         where x = head unusedVars
               y = unusedVars !! 1
               unusedVars = notUsed $ allVar n
sum :: Term -> Term -> Term
sum a b = Apply sum_a b
          where sum_a = rec a (Lambda x (VarTerm x))
                            (Lambda n (Lambda res
                                (Lambda x (succ (Apply (VarTerm res) (VarTerm x))))
                            ))
                x = head unusedVars
                n = unusedVars !! 1
                res = unusedVars !! 2
                unusedVars = notUsed $ unions [allVar a, allVar b]

-- Interpretazione
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
