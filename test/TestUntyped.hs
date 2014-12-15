module TestUntyped where

import Prelude hiding (succ, sum)
import Test.HUnit
import Data.Maybe

import Variable
import Lambda
import Untyped

tests :: Test
tests = test [
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
            "Test proj"
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
            "Test succ 1"
            (Lambda (Variable "b")
                (Apply (Apply
                    (VarTerm (Variable "b"))
                    (Lambda (Variable "b") (Lambda (Variable "a") (VarTerm (Variable "a")))))
                    (Lambda (Variable "x") (VarTerm (Variable "x")))))
            (reduceAll (Apply succ zero))
        ,
        assertEqual
            "Test sum 0+0"
            zero
            (reduceAll (Apply (Apply sum zero) zero))
        ,
        assertEqual
            "Test sum 1+0"
            (reduceAll (Apply succ zero))
            (reduceAll (Apply (Apply sum (Apply succ zero)) zero))
        ,
        assertEqual
            "Test sum 0+1"
            (reduceAll (Apply succ zero))
            (reduceAll (Apply (Apply sum zero) (Apply succ zero)))
        ,
        assertEqual
            "Test sum 1+1"
            (reduceAll (Apply succ (Apply succ zero)))
            (reduceAll (Apply (Apply sum (Apply succ zero)) (Apply succ zero)))
    ]
    where
        f = VarTerm (Variable "f")
