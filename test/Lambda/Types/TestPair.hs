module Lambda.Types.TestPair where

import Prelude hiding (succ, pred, sum)
import Test.HUnit

import Lambda.Variable
import Lambda.Engine
import Lambda.Types.Pair
import Lambda.Types.Bool
import Lambda.Types.Int

tests :: Test
tests = test [
        assertEqual
            "Test left pair"
            (VarTerm (Variable "left"))
            (reduceAll (left (pair
                (VarTerm (Variable "left"))
                (VarTerm (Variable "right")))))
        ,
        assertEqual
            "Test right pair"
            (VarTerm (Variable "right"))
            (reduceAll (right (pair
                (VarTerm (Variable "left"))
                (VarTerm (Variable "right")))))
        ,
        assertEqual
            "Test showPair false false"
            "<false,false>"
            (showPair showBool showBool (pair false false))
        ,
        assertEqual
            "Test showPair true true"
            "<true,true>"
            (showPair showBool showBool (pair true true))
        ,
        assertEqual
            "Test showPair <false,false> <true,true>"
            "<<false,false>,<true,true>>"
            (showPair
                (showPair showBool showBool)
                (showPair showBool showBool)
                (pair
                    (pair false false)
                    (pair true true)))
        ,
        assertEqual
            "Test showPair one false"
            "<succ(zero),false>"
            (showPair showInt showBool (pair one false))
        ,
        assertEqual
            "Test showPair three two"
            "<succ(succ(succ(zero))),succ(succ(zero))>"
            (showPair showInt showInt (pair three two))
    ]
    where
        one = succ zero
        two = succ one
        three = succ two
