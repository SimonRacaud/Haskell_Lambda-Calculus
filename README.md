# Haskell Lambda Calculus

This is a parser for complex [Lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus) expressions including parsers for:
- long and short lambda functions
> Example: "λx.x(λy.y)(λz.zz)"
- logical lambda expressions (or, and, not, if)
> Example: "if True and not False then True or True else False"
- arithmetic expressions (sum, subtraction, exponentiation, product)
> Example: "2* 2 **(8 - 1)"
- comparison expressions (>=, <=, <, >, ==, !=)
> Example: "15 - 2 * 2 != 2**3 + 3 or 5 * 3 + 1 < 9"
- list of expressions and the possibility to apply functions to that list.
> Example: "head cons 42 rest rest [42, 43, 44, 45]"

> Result: "42" (in lambda calculus)

## Documentation:

See *ProjectReport.pdf* for a documentation of the project.

## Examples:

```
-- >>> parse longLambdaP "(λx.xx)"
-- Result >< \x.xx
--
-- >>> parse longLambdaP "(λx.(λy.xy(xx)))"
-- Result >< \xy.xy(xx)
--
-- >>> parse longLambdaP "(λx(λy.x))"
-- UnexpectedChar '('
--
-- >>> parse longLambdaP "(λx.xx)(λx.xx)"
-- Result >< (\x.xx)\x.xx
--
-- >>> parse longLambdaP "(λxy.xy)"
-- UnexpectedChar '('
--
-- >>> parse longLambdaP "λy.(λx.xx)y"
-- Result >< \y.(\x.xx)y

-- >>> lamToBool <$> parse complexCalcP "True and not not not False"
-- Result >< Just True
--
-- >>> lamToBool <$> parse complexCalcP "True and not not False"
-- Result >< Just False
--
-- >>> lamToBool <$> parse complexCalcP "True or not not False"
-- Result >< Just True
--
-- >>> lamToBool <$> parse complexCalcP "True or False and not not False or False"
-- Result >< Just True
--
-- >>> lamToBool <$> parse complexCalcP "if True and not False then if True and not False then True or True else False else False"
-- Result >< Just True
--
-- >>> lamToBool <$> parse complexCalcP "if True and not False then (if True and not False then True or True else False) else False"
-- Result >< Just True
--
-- >>> lamToBool <$> parse complexCalcP "not  True and False or  True"
-- Result >< Just True
--
-- >>> lamToBool <$> parse complexCalcP "True or (  True and False ) or not False"
-- Result >< Just True
--
-- >>> lamToBool <$> parse complexCalcP "True or False or not False"

-- >>> lamToInt <$> parse arithmeticP "1 + 8 * 8 + 2 * 8"
-- Result >< Just 81
--
-- >>> lamToInt <$> parse arithmeticP "1 - 2"
-- Result >< Just 0
--
-- >>> lamToInt <$> parse arithmeticP "2 **8"
-- Result >< Just 256
--
-- >>> lamToInt <$> parse arithmeticP "2 **8 - 1"
-- Result >< Just 255
--
-- >>> lamToInt <$> parse arithmeticP "2 **(8 - 1)"
-- Result >< Just 128

-- >>> parse listP "[]"
-- Result >< \cn.n
--
-- >>> parse listP "[True]"
-- Result >< (\htcn.ch(tcn))(\xy.x)\cn.n
--
-- >>> parse listP "[0, 0]"
-- Result >< (\htcn.ch(tcn))(\fx.x)((\htcn.ch(tcn))(\fx.x)\cn.n)
--
-- >>> parse listP "[0, 0"
-- UnexpectedEof
--
-- >>> parse listP "[True, False, True, False, False]"
-- Result >< (\htcn.ch(tcn))(\xy.x)((\htcn.ch(tcn))(\xy.y)((\htcn.ch(tcn))(\xy.x)((\htcn.ch(tcn))(\xy.y)((\htcn.ch(tcn))(\xy.y)\cn.n))))

-- >>> lamToBool <$> parse listOpP "head [(λxy.x)]"
-- Result >< Just True
--
-- >>> lamToBool <$> parse listOpP "head [(λxy.y)]"
-- Result >< Just False
--
-- >>> lamToInt <$> parse listOpP "head rest rest [42, 43, 44, 45]"
-- Result >< Just 44
--
-- >>> lamToInt <$> parse listOpP "head cons 8 rest rest [42, 43, 44, 45]"
-- Result >< Just 8
```

See *submission/LambdaParser.hs* for more examples of usage.

## Using the code bundle

`stack build`

Builds the packages.

`stack run`

Builds an executable that runs the main function in app/Main.hs.

`stack test`

Builds the packages and executes doctests on all hs files in the submission folder.

`stack clean --full`

Removes unnecessary build files to reduce bundle size.

## Troubleshooting

`/usr/bin/ld.gold: error: cannot find -lgmp`

Run `sudo apt-get install libgmp3-dev`
