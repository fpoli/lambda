module Types.Either where

import Variable
import Lambda

-- Introduzione
inl :: Term -> Term
inl a = Lambda onleft (Lambda onright (Apply (VarTerm onleft) a))
        where onleft = head unusedVars
              onright = unusedVars !! 1
              unusedVars = notUsed $ allVar a
inr :: Term -> Term
inr b = Lambda onleft (Lambda onright (Apply (VarTerm onright) b))
        where onleft = head unusedVars
              onright = unusedVars !! 1
              unusedVars = notUsed $ allVar b

-- Eliminazione
reveal :: Term -> Term -> Term -> Term
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
            _ ->
                show term
