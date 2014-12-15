module Types.TestInt where

import Prelude hiding (succ, pred, sum)
import Test.HUnit

import Types.Int
import Types.Bool
import Lambda

tests :: Test
tests = test [
        assertEqual
            "Test eqz zero"
            true
            (reduceAll (eqz zero))
        ,
        assertEqual
            "Test eqz succ(zero)"
            false
            (reduceAll (eqz (succ zero)))
        ,
        assertEqual
            "Test pred zero"
            true
            (reduceAll (pred zero))
        ,
        assertEqual
            "Test pred succ(zero)"
            zero
            (reduceAll (pred (succ zero)))
        ,
        assertEqual
            "Test pred succ(succ(zero))"
            (reduceAll (succ zero))
            (reduceAll (pred (succ (succ zero))))
        ,
        assertEqual
            "Test sum zero zero"
            zero
            (reduceAll (sum zero zero))
        ,
        assertEqual
            "Test sum zero succ(zero)"
            (reduceAll (succ zero))
            (reduceAll (sum zero (succ zero)))
        ,
        assertEqual
            "Test sum succ(zero) zero"
            (reduceAll (succ zero))
            (reduceAll (sum (succ zero) zero))
        ,
        assertEqual
            "Test sum succ(zero) succ(zero)"
            (reduceAll ((succ.succ) zero))
            (reduceAll (sum (succ zero) (succ zero)))
        ,
        assertEqual
            "Test sum succ.succ(zero) succ(zero)"
            (reduceAll ((succ.succ.succ) zero))
            (reduceAll (sum ((succ.succ) zero) (succ zero)))
        ,
        assertEqual
            "Test showInt succ(zero)"
            "succ(zero)"
            (showInt (succ zero))
        ,
        assertEqual
            "Test showInt succ(succ(zero))"
            "succ(succ(zero))"
            (showInt (succ (succ zero)))
        ,
        assertEqual
            "Test showInt reduceAll succ(zero)"
            "succ(zero)"
            (showInt (reduceAll (succ zero)))
        ,
        assertEqual
            "Test showInt reduceAll succ(succ(zero))"
            "succ(succ(zero))"
            (showInt (reduceAll (succ (succ zero))))
        ,
        assertEqual
            "Test showInt reduceAll zero+zero"
            "zero"
            (showInt (sum zero zero))
        ,
        assertEqual
            "Test showInt reduceAll zero+one"
            "succ(zero)"
            (showInt (sum zero one))
        ,
        assertEqual
            "Test showInt reduceAll three+one"
            "succ(succ(succ(succ(zero))))"
            (showInt (sum three one))
    ]
    where
        one = succ zero
        two = succ one
        three = succ two
