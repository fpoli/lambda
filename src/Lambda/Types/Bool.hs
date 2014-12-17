module Lambda.Types.Bool where

import Lambda.Engine
import Lambda.Parser

-- Introduction
true :: Term
true = parseRaw "λx.λy.x"
false :: Term
false = parseRaw "λx.λy.y"

-- Elimination
ifthenelse :: Term -> Term -> Term -> Term
ifthenelse test ontrue onfalse = Apply (Apply test ontrue) onfalse

-- Interpretation
showBool :: Term -> String
showBool t =
    let
        term = reduceAll t
    in
        if term == true
        then "true"
        else
            if term == false
            then "false"
            else show term
