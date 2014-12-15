module Types.Either where

import Data.Set (unions)
import Variable
import Lambda
import Parser

-- Introduzione
inl a = Lambda onleft (Lambda onright (Apply (VarTerm onleft) a))
        where onleft = head unusedVars
              onright = unusedVars !! 1
              unusedVars = notUsed $ allVar a
inr b = Lambda onleft (Lambda onright (Apply (VarTerm onright) b))
        where onleft = head unusedVars
              onright = unusedVars !! 1
              unusedVars = notUsed $ allVar b

-- Distruzione
reveal x onleft onright = Apply (Apply x onleft) onright

-- Interpretazione
showEither :: (Term -> String) -> (Term -> String) -> Term -> String
showEither showLeft showRight t =
    let
        onleft = Variable "onleft"
        onright = Variable "onright"
        term = reduceAll $ applyArgs t $ map VarTerm [onleft, onright]
    in
        case explode term of
            (VarTerm x, args) | x == onleft ->
                let
                    left = applyArgs (head args) (tail args)
                in
                    "inl(" ++  showLeft left ++ ")"
            (VarTerm x, args) | x == onright ->
                let
                    right = applyArgs (head args) (tail args)
                in
                    "inr(" ++  showRight right ++ ")"
            otherwise ->
                show term
