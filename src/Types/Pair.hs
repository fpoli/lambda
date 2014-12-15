module Types.Pair where

import Data.Set (unions)
import Variable
import Lambda
import Parser

-- Introduzione
pair :: Term -> Term -> Term
pair a b = Lambda x (Apply (Apply (VarTerm x) a) b)
           where x = head $ notUsed $ unions [allVar a, allVar b]

-- Eliminazione
split :: Term -> Term -> Term
split a f = Apply a f

-- Utilità
left :: Term -> Term
left a = split a (Lambda x (Lambda y (VarTerm x)))
         where x = head unusedVars
               y = unusedVars !! 1
               unusedVars = notUsed $ allVar a

right :: Term -> Term
right a = split a (Lambda x (Lambda y (VarTerm y)))
          where x = head unusedVars
                y = unusedVars !! 1
                unusedVars = notUsed $ allVar a

-- Interpretazione
showPair :: (Term -> String) -> (Term -> String) -> Term -> String
showPair showA showB t =
    let
        onpair = Variable "onpair"
        term = reduceAll $ Apply t (VarTerm onpair)
    in
        case explode term of
            (VarTerm x, args) | x == onpair ->
                let
                    a = applyArgs (parseRaw "λa.λb.a") args
                    b = applyArgs (parseRaw "λa.λb.b") args
                in
                    "<" ++  showA a ++ "," ++ showB b ++ ">"
            _ ->
                show term
