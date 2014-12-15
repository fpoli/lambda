import System.Exit
import Control.Monad
import Test.HUnit

import TestVariable
import TestLambda
import TestParser
import TestUntyped
import Types.TestBool
import Types.TestInt
import Types.TestPair
import Types.TestEither

main :: IO ()
main = do
    count <- runTestTT $ TestList [
            TestVariable.tests,
            TestLambda.tests,
            TestParser.tests,
            TestUntyped.tests,
            Types.TestBool.tests,
            Types.TestInt.tests,
            Types.TestPair.tests,
            Types.TestEither.tests
        ]
    when (errors count /= 0 || failures count /= 0)
		exitFailure
