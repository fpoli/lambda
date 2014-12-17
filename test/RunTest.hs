import System.Exit
import Control.Monad
import Test.HUnit

import Lambda.TestVariable
import Lambda.TestEngine
import Lambda.TestParser
import Lambda.TestUntyped
import Lambda.Types.TestBool
import Lambda.Types.TestInt
import Lambda.Types.TestPair
import Lambda.Types.TestEither

main :: IO ()
main = do
    count <- runTestTT $ TestList [
            Lambda.TestVariable.tests,
            Lambda.TestEngine.tests,
            Lambda.TestParser.tests,
            Lambda.TestUntyped.tests,
            Lambda.Types.TestBool.tests,
            Lambda.Types.TestInt.tests,
            Lambda.Types.TestPair.tests,
            Lambda.Types.TestEither.tests
        ]
    when (errors count /= 0 || failures count /= 0)
        exitFailure
