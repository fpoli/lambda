import Prelude hiding (succ, sum)
import Data.Maybe

import Lambda
import Parser
import Types.Int

main :: IO()
main = do
	let one = succ zero
	let two = succ one
	let three = succ two
	let four = succ three
	let six = sum four two
	let ten = sum four six

	let compiled = reduceAll ten
	print compiled
	-- output: ...
	putStrLn $ showInt compiled
	-- output: succ(succ(...(succ(zero))...))

	let term = fromJust $ parse "λx.λy.(λz.z(y))(x)"
	print term
	-- output: λx.λy.(λz.z(y))(x)
	print $ reduceAll term
	-- output: λx.λy.x(y)
