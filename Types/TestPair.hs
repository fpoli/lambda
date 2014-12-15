#!/usr/bin/env runhaskell

import Types.Pair
import Test.HUnit
import Variable
import Lambda
import Prelude hiding (succ, pred, sum)
import Types.Bool
import Types.Int

main = do
    let one = succ zero
    let two = succ one
    let three = succ two
    runTestTT (test [
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
        ])
