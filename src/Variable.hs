module Variable where

import qualified Data.Set as Set

data Variable = Variable String deriving (Eq, Ord)

toString :: Variable -> String
toString (Variable x) = x

instance Show Variable where
    show (Variable x) = x

allCharacters :: String
allCharacters = ['a'..'z']

allStrings :: [String]
allStrings = [[c] | c <- allCharacters]
             ++
             [s++[c] | s <- allStrings, c <- allCharacters]

allVariables :: [Variable]
allVariables = map Variable allStrings

notUsed :: Set.Set Variable -> [Variable]
notUsed used
    | Set.null used = allVariables
    | otherwise = [s | s <- allVariables, not (Set.member s used)]
