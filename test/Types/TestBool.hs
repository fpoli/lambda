module Types.TestBool where

import Test.HUnit

import Types.Bool
import Variable
import Lambda

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
