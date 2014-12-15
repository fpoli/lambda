#!/usr/bin/env runhaskell

import Prelude hiding (succ, pred, sum)
-- import Variable
import Lambda
-- import Parser
-- import Untyped
import Types.Int
-- import Hash

main :: IO()
main = do
    let one = succ zero
    let two = succ one
    let three = succ two
    let four = succ three
    let six = sum four two
    let ten = sum four six

    let compiled = reduceAll ten

    putStrLn ("Compiled:    " ++ show compiled)
    putStrLn ("Interpreted: " ++ showInt compiled)
