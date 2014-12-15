# Lambda

Untyped Lambda Calculus Machine, with functions for encoding simple datatypes (`Int`, `Bool`, `Either`, `Pair`) back and forth to the lambda-representation.

## Example

```haskell
import Prelude hiding (succ, pred, sum)
import Lambda
import Types.Int

let two = succ $ succ zero
print two
-- λc.λd.d(λa.λb.b(λx.λy.x)((λx.λy.x)(a)(b)))((λa.λb.b(λx.λy.x)((λx.λy.x)(a)(b)))(c)(d))

print $ reduceAll two
-- λc.λd.d(λa.λb.b(λx.λy.x)(a))(d(λc.λa.c)(c))

putStrLn showInt two
-- succ(succ(zero))
```

See `main.hs`.

## License

Copyright (C) 2014 Federico Poli <federpoli@gmail.com>

Released under the GNU General Public License, version 3.
