module Lambda.TestEngine where

import Test.HUnit
import qualified Data.Set as Set

import Lambda.Variable
import Lambda.Engine
import Lambda.Parser

tests :: Test
tests = test [
        assertEqual
            "Test uguaglianza Term 1"
            True
            (parseRaw "λz.x" == parseRaw "λk.x")
        ,
        assertEqual
            "Test uguaglianza Term 2"
            False
            (parseRaw "λa.λb.a" == parseRaw "λa.λb.b")
        ,
        assertEqual
            "Test uguaglianza Term 3"
            True
            (parseRaw "λa.λb.b" == parseRaw "λc.λc.c")
        ,
        assertEqual
            "Test uguaglianza Term 4"
            True
            (parseRaw "λb.λb.b" == parseRaw "λc.λc.c")
        ,
        assertEqual
            "Test uguaglianza Term 5"
            False
            (parseRaw "λx.x(y)" == parseRaw "λy.y(y)")
        ,
        assertEqual
            "Test substitute"
            (parseRaw "λa.a(λz.x)")
            (substitute
                (parseRaw "λx.x(y)")
                (Variable "y")
                (parseRaw "λz.x"))
        ,
        assertEqual
            "Test allVar 1"
            (Set.fromList [Variable "a", Variable "x", Variable "z"])
            (allVar (parseRaw "λa.a(λz.x)"))
        ,
        assertEqual
            "Test allVar 2"
            (Set.fromList [Variable "x", Variable "y"])
            (allVar (parseRaw "λx.λy.x"))
        ,
        assertEqual
            "Test betaReduce"
            (Just (parseRaw "a"))
            (betaReduce (parseRaw "(λb.a)(x)"))
        ,
        assertEqual
            "Test betaReduce"
            (Just (parseRaw "(λret.λa.ret)(y2)(z3)"))
            (betaReduce (parseRaw "(λb.λret.λa.ret)(x1)(y2)(z3)"))
        ,
        assertEqual
            "Test reduceAll"
            (parseRaw "x")
            (reduceAll (parseRaw "(λy.y)(x)"))
        ,
        assertEqual
            "Test applyArgs"
            (parseRaw "f(x1)(x2)(x3)")
            (applyArgs
                (parseRaw "f")
                [parseRaw "x1",
                 parseRaw "x2",
                 parseRaw "x3"])
        ,
        assertEqual
            "Test lambdaVars"
            (parseRaw "λx1.λx2.λx3.m")
            (lambdaVars
                [Variable "x1",
                 Variable "x2",
                 Variable "x3"]
                (parseRaw "m"))
    ]
