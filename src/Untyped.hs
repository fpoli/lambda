module Untyped where

import Prelude hiding (succ)
import qualified Data.Set as Set
import Data.Set (unions)
import Variable
import Lambda
import Parser

-- omega --> omega --> ...
omega :: Term
omega = parseRaw "(λx.x(x))(λx.x(x))"

-- y = fix
-- y(f) --> z=y(f) --> f(y(f)) --> f(f(y(f))) --> ... e sono tutti = f(y(f))
-- Dato f = tau(f)
-- allora f = fix(tau)
-- e lo ricaviamo con f = y(tau) --> tau(y(tau)) = tau(f)
fix :: Term
fix = parseRaw "λf.(λx.f(x(x)))(λx.f(x(x)))"

-- restituisce il primo o il secondo
k1 :: Term
k1 = parseRaw "λx.λy.x"
k2 :: Term
k2 = parseRaw "λx.λy.y"

-- Booleani
true :: Term
true  = k1
false :: Term
false = k2
ifThenElse :: Term -> Term -> Term -> Term
ifThenElse c d e = Apply (Apply c d) e

-- Coppie
pair :: Term -> Term -> Term
pair a b = Lambda x (Apply (Apply (VarTerm x) a) b)
           -- allVar evita di collidere con variabili libere
           -- si poteva usare anche freeVar
           where x = head (notUsed (unions [allVar a, allVar b]))
p1 :: Term -> Term
p1 c = Apply c k1
p2 :: Term -> Term
p2 c = Apply c k2

-- Funzioni elementari
ident :: Term
ident = parseRaw "λx.x"

zero :: Term
zero = ident

zfun :: Term
zfun = Lambda (Variable "x") zero

succ :: Term
succ = Lambda (Variable "x") (pair false (VarTerm (Variable "x")))

eqz :: Term
eqz = Lambda (Variable "x") (p1 (VarTerm (Variable "x")))

predec :: Term
predec = Lambda (Variable "x") (p2 (VarTerm (Variable "x")))

--      n      i   -> P^n_i
proj :: Int -> Int -> Term
proj 0 _ = VarTerm (Variable "ret")
proj n 1 = Lambda (Variable "ret") (proj (n-1) 0)
proj n i = Lambda (head (notUsed (allVar inner))) inner
           where inner = proj (n-1) (i-1)
-- allVar evita di collidere con variabili libere
-- si poteva usare anche freeVar

--      n      F^m     G1..Gm -> F(G1(x1..xn)..Gm(x1..xn))
comp :: Int -> Term -> [Term] -> Term
comp n f g_list = foldr Lambda
                        (applyArgs f (map (\g->applyArgs g (map VarTerm args)) g_list))
                        args
                  where args = take n (notUsed
                                        (unions [
                                            allVar x |
                                            x <- f:g_list
                                        ]))

rec :: Term -> Term -> Term
rec k g = Apply
            fix (Lambda f
                (Lambda x
                    (Lambda y
                        (ifThenElse
                            (Apply eqz (VarTerm x))
                            (Apply k (VarTerm y))
                            (Apply(Apply(Apply g
                                (Apply predec (VarTerm x)))
                                (VarTerm y))
                                (Apply(Apply (VarTerm f)
                                    (Apply predec (VarTerm x)))
                                    (VarTerm y)
                                )
                            )
                        )
                    )
                )
            )
          where x = head (notUsed (unions [allVar k, allVar g]))
                y = head (notUsed (unions [allVar k, allVar g, Set.fromList [x]]))
                f = head (notUsed (unions [allVar k, allVar g, Set.fromList [x, y]]))

-- num arguments?
min :: Term -> Term
min _ = error "TODO"

--
sum :: Term
sum = rec ident (comp 3 succ [proj 3 3])
