module Lambda.Types.TestEither where

import Prelude hiding (succ, pred, sum)
import Test.HUnit

import Lambda.Variable
import Lambda.Engine
import Lambda.Types.Either
import Lambda.Types.Bool
import Lambda.Types.Int
import Lambda.Types.Pair

tests :: Test
tests = test [
        assertEqual
            "Test reveal inl"
            (VarTerm (Variable "left"))
            (reduceAll (reveal
                (inl (VarTerm (Variable "left")))
                (Lambda (Variable "x") (VarTerm (Variable "x")))
                (Lambda (Variable "x") (VarTerm (Variable "wrong")))))
        ,
        assertEqual
            "Test reveal inr"
            (VarTerm (Variable "right"))
            (reduceAll (reveal
                (inr (VarTerm (Variable "right")))
                (Lambda (Variable "x") (VarTerm (Variable "wrong")))
                (Lambda (Variable "x") (VarTerm (Variable "x")))))
        ,
        assertEqual
            "Test showEither inl(zero)"
            "inl(zero)"
            (showEither showInt showInt (inl zero))
        ,
        assertEqual
            "Test showEither inr(true)"
            "inr(true)"
            (showEither show showBool (inr true))
        ,
        assertEqual
            "Test showEither inl(one)"
            "inl(succ(zero))"
            (showEither showInt show (inl one))
        ,
        assertEqual
            "Test showEither inr(<true,two>)"
            "inr(<true,succ(succ(zero))>)"
            (showEither
                showInt
                (showPair showBool showInt)
                (inr (pair true two)))
    ]
    where
        one = succ zero
        two = succ one
