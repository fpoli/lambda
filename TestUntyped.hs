#!/usr/bin/env runhaskell

import Test.HUnit
import Data.Maybe
import Variable
import Lambda
import Untyped

main =
    runTestTT (test [
            assertEqual
                "Test Omega"
                (Just omega)
                (betaReduce omega)
            ,
            assertEqual
                "Test fix"
                (Just (Apply f (fromJust (betaReduce (Apply fix f)))))
                (betaReduce (fromJust (betaReduce (Apply fix f))))
            ,
            assertEqual
                "Test proj "
                (VarTerm (Variable "y2"))
                (reduceAll
                    (applyArgs (proj 3 2)
                        [VarTerm (Variable "x1"),
                         VarTerm (Variable "y2"),
                         VarTerm (Variable "z3")]))
            ,
            assertEqual
                "Test comp1"
                (applyArgs
                    (VarTerm (Variable "f"))
                    [VarTerm (Variable "g1"),
                     VarTerm (Variable "g2"),
                     VarTerm (Variable "g3")])
                (comp 0
                    (VarTerm (Variable "f"))
                    [VarTerm (Variable "g1"),
                     VarTerm (Variable "g2"),
                     VarTerm (Variable "g3")])
            ,
            assertEqual
                "Test comp2"
                (Lambda (Variable "a") (Lambda (Variable "b")
                    (Apply (Apply (Apply
                        (VarTerm (Variable "f"))
                        (Apply (Apply (VarTerm (Variable "g1")) (VarTerm (Variable "a"))) (VarTerm (Variable "b"))))
                        (Apply (Apply (VarTerm (Variable "g2")) (VarTerm (Variable "a"))) (VarTerm (Variable "b"))))
                        (Apply (Apply (VarTerm (Variable "g3")) (VarTerm (Variable "a"))) (VarTerm (Variable "b"))))))
                (comp 2
                    (VarTerm (Variable "f"))
                    [VarTerm (Variable "g1"),
                     VarTerm (Variable "g2"),
                     VarTerm (Variable "g3")])
            ,
            assertEqual
                "Test somma 1"
                (Lambda (Variable "b")
                    (Apply (Apply
                        (VarTerm (Variable "b"))
                        (Lambda (Variable "b") (Lambda (Variable "a") (VarTerm (Variable "a")))))
                        (Lambda (Variable "x") (VarTerm (Variable "x")))))
                (reduceAll (Apply success zero))
            ,
            assertEqual
                "Test somma 0+0"
                zero
                (reduceAll (Apply (Apply somma zero) zero))
            ,
            assertEqual
                "Test somma 1+0"
                (reduceAll (Apply success zero))
                (reduceAll (Apply (Apply somma (Apply success zero)) zero))
            ,
            assertEqual
                "Test somma 0+1"
                (reduceAll (Apply success zero))
                (reduceAll (Apply (Apply somma zero) (Apply success zero)))
            ,
            assertEqual
                "Test somma 1+1"
                (reduceAll (Apply success (Apply success zero)))
                (reduceAll (Apply (Apply somma (Apply success zero)) (Apply success zero)))
        ])
    where
        f = VarTerm (Variable "f")
        ide = Lambda (Variable "x") (VarTerm (Variable "x"))
