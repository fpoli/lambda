module TestParser where

import Test.HUnit

import Variable
import Lambda
import Parser

tests :: Test
tests = test [
        assertEqual
            "Test parse x"
            (Just (VarTerm (Variable "x")))
            (parse "x")
        ,
        assertEqual
            "Test parse ab"
            (Just (VarTerm (Variable "ab")))
            (parse "ab")
        ,
        assertEqual
            "Test parse ab12"
            (Just (VarTerm (Variable "ab12")))
            (parse "ab12")
        ,
        assertEqual
            "Test parse (x"
            Nothing
            (parse "(x")
        ,
        assertEqual
            "Test parse .x"
            Nothing
            (parse ".x")
        ,
        assertEqual
            "Test parse x$pippo"
            Nothing
            (parse "x$pippo")
        ,
        assertEqual
            "Test parse (x)"
            (Just (VarTerm (Variable "x")))
            (parse "(x)")
        ,
        assertEqual
            "Test parse x(y)"
            (Just (Apply
                (VarTerm (Variable "x"))
                (VarTerm (Variable "y"))))
            (parse "x(y)")
        ,
        assertEqual
            "Test parse (x)(y)"
            (Just (Apply
                (VarTerm (Variable "x"))
                (VarTerm (Variable "y"))))
            (parse "(x)(y)")
        ,
        assertEqual
            "Test parse (x)(y)(z)"
            (Just (Apply
                (Apply
                    (VarTerm (Variable "x"))
                    (VarTerm (Variable "y")))
                (VarTerm (Variable "z"))))
            (parse "(x)(y)(z)")
        ,
        assertEqual
            "Test parse (x)((y)(z))"
            (Just (Apply
                (VarTerm (Variable "x"))
                (Apply
                    (VarTerm (Variable "y"))
                    (VarTerm (Variable "z")))))
            (parse "(x)((y)(z))")
        ,
        assertEqual
            "Test parse λx.m"
            (Just (Lambda (Variable "x")
                (VarTerm (Variable "m"))))
            (parse "λx.m")
        ,
        assertEqual
            "Test parse λx.λy.z"
            (Just (Lambda (Variable "x")
                (Lambda (Variable "y")
                    (VarTerm (Variable "z")))))
            (parse "λx.λy.z")
        ,
        assertEqual
            "Test parse λx.λy.z(a)"
            (Just (Lambda (Variable "x")
                (Lambda (Variable "y")
                    (Apply
                        (VarTerm (Variable "z"))
                        (VarTerm (Variable "a"))))))
            (parse "λx.λy.z(a)")
        ,
        assertEqual
            "Test parse λx.x(y)"
            (Just (Lambda (Variable "x")
                (Apply
                    (VarTerm (Variable "x"))
                    (VarTerm (Variable "y")))))
            (parse "λx.x(y)")
        ,
        assertEqual
            "Test parse (λy.y)(x)"
            (Just (Apply
                (Lambda (Variable "y")
                    (VarTerm (Variable "y")))
                (VarTerm (Variable "x"))))
            (parse "(λy.y)(x)")
        ,
        assertEqual
            "Test parse (λx.x(x))(λx.x(x))"
            (Just (Apply
                (Lambda (Variable "x")
                    (Apply
                        (VarTerm (Variable "x"))
                        (VarTerm (Variable "x"))))
                (Lambda (Variable "x")
                        (Apply
                            (VarTerm (Variable "x"))
                            (VarTerm (Variable "x"))))))
            (parse "(λx.x(x))(λx.x(x))")
    ]
