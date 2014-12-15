# Lambda

Untyped Lambda Calculus Machine, with functions for encoding simple datatypes (`Int`, `Bool`, `Either`, `Pair`) back and forth their lambda-representation.

## Example

```haskell
import Prelude hiding (succ)
import Lambda
import Types.Int

main :: IO()
main = do
	let two = succ $ succ zero
	print two
	-- output: λc.λd.d(λa.λb.b(λx.λy.x)((λx.λy.x)(a)(b)))((λa.λb.b(λx.λy.x)((λx.λy.x)(a)(b)))(c)(d))

	print $ reduceAll two
	-- output: λc.λd.d(λa.λb.b(λx.λy.x)(a))(d(λc.λa.c)(c))

	putStrLn $ showInt two
	-- output: succ(succ(zero))
```

## License

Copyright (C) 2014 Federico Poli <federpoli@gmail.com>

Released under the GNU General Public License, version 3.
