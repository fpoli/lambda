module Types.Bool where

import Lambda
import Parser

-- Introduzione
true :: Term
true = parseRaw "λx.λy.x"
false :: Term
false = parseRaw "λx.λy.y"

-- Distruzione
ifthenelse :: Term -> Term -> Term -> Term
ifthenelse test ontrue onfalse = Apply (Apply test ontrue) onfalse

-- Interpretazione
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
