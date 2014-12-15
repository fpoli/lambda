module Hash where

import Data.List
import Data.Maybe
import Lambda
import Variable

pack :: Integer -> Integer -> Integer
pack 0 0 = 0
pack a b = (a + b) * (a + b + 1) `div` 2 + b

unpack :: Integer -> (Integer, Integer)
unpack n =
    let
        b = sqrt $ fromIntegral (8 * n + 1) :: Double
        w = (truncate b - 1) `div` 2
    in (((w * (w + 3)) `div` 2) - n, n - ((w * (w + 1)) `div` 2))

hashVariable :: Variable -> Integer
hashVariable x = fromIntegral $ fromJust $ elemIndex x (take 1000 allVariables)

reverseVariable :: Integer -> Variable
reverseVariable n = allVariables !! fromIntegral n

hashTerm :: Term -> Integer
hashTerm (VarTerm x)  = 0 + 3 * hashVariable x
hashTerm (Lambda x m) = 1 + 3 * pack (hashVariable x) (hashTerm m)
hashTerm (Apply m1 m2)  = 2 + 3 * pack (hashTerm m1) (hashTerm m2)

reverseTerm :: Integer -> Term
reverseTerm n
    | n `mod` 3 == 0 =
        let x = n `div` 3
        in VarTerm (reverseVariable x)
    | n `mod` 3 == 1 =
        let (x, m) = unpack (n `div` 3)
        in Lambda (reverseVariable x) (reverseTerm m)
    | otherwise =
        let (m1, m2) = unpack (n `div` 3)
        in Apply (reverseTerm m1) (reverseTerm m2)
