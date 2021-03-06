module Lambda.Types.TestBool where

import Test.HUnit

import Lambda.Variable
import Lambda.Engine
import Lambda.Types.Bool

tests :: Test
tests = test [
            assertEqual
            "Test ifthenelse true"
            (VarTerm (Variable "first"))
            (reduceAll (ifthenelse true (VarTerm (Variable "first")) (VarTerm (Variable "second"))))
        ,
        assertEqual
            "Test ifthenelse false"
            (VarTerm (Variable "second"))
            (reduceAll (ifthenelse false (VarTerm (Variable "first")) (VarTerm (Variable "second"))))
        ,
        assertEqual
            "Test showBool true"
            "true"
            (showBool true)
        ,
        assertEqual
            "Test showBool false"
            "false"
            (showBool false)
    ]
