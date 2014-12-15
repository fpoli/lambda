#!/usr/bin/env runhaskell

import Test.HUnit
import Variable
import qualified Data.Set as Set

-- Data una funzione f:A->B e una coppia (a,b) controlla che f(a)=b
unitTest :: (Show a, Show b, Eq b) => String -> (a -> b) -> [(a, b)] -> Test
unitTest title f pairs = test [ assertEqual (title ++ ": testing input " ++ show a) b (f a) | (a, b) <- pairs ]

allcharactersTests = unitTest
    "allCharacters content"
    (`elem` allCharacters)
    [
        ('a', True),
        ('c', True),
        ('z', True),
        ('A', False),
        ('C', False),
        ('Z', False),
        ('?', False)
    ]

allVariablesContentTests = unitTest
    "allVariables content (first 1000 elements)"
    (`elem` allVariables) -- TODO filtra i primi 100
    [ (Variable a, b) | (a, b) <- [
            ("a", True),
            ("b", True),
            ("z", True),
            ("aa", True),
            ("ab", True),
            ("ac", True),
            ("cw", True){-,
            ("", False),
            (" ", False),
            ("a a", False),
            ("1", False),
            ("a1b", False)
-}    ]]

allVariablesOrderTests = unitTest
    "allVariables order"
    (allVariables !!) -- TODO controlla l'ordine
    [ (a, Variable b) | (a, b) <- [
            (0, "a"),
            (1, "b"),
            (25, "z"),
            (26, "aa"),
            (27, "ab"),
            (28, "ac"),
            (100, "cw")
    ]]

notUsedTests = unitTest
    "notUsed"
    (head . notUsed . Set.fromList)
    [ (map Variable a, Variable b) | (a, b) <- [
            ([], "a"),
            (["b"], "a"),
            (["aa"], "a"),
            (["b", "c", "d"], "a"),
            (["a"], "b"),
            (["a", "c"], "b"),
            (["a", "c", "d"], "b"),
            (["a", "c", "d", "aa"], "b"),
            (["a", "b", "c", "d"], "e"),
            ([[x] | x <- allCharacters], "aa"),
            ([[x] | x <- allCharacters]++["aa"], "ab"),
            ([[x] | x <- allCharacters]++['a':[x] | x <- allCharacters], "ba")
    ]]

main =
    runTestTT (test [
            allcharactersTests,
            allVariablesContentTests,
            allVariablesOrderTests,
            notUsedTests
        ])
