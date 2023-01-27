# Haskell Lambda Calculus

This is a parser for complex [Lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus) expressions including parsers for:
- long and short lambda functions
> Example: "λx.x(λy.y)(λz.zz)"
- logical lambda expressions (or, and, not, if)
> Example: "if True and not False then True or True else False"
- arithmetic expressions (sum, substraction, exponentiation, product)
> Example: "2* 2 **(8 - 1)"
- comparison expressions (>=, <=, <, >, ==, !=)
> Example: "15 - 2 * 2 != 2**3 + 3 or 5 * 3 + 1 < 9"
- list of expressions and the possibility to apply functions to that list.
> Example: "head cons 42 rest rest [42, 43, 44, 45]"

> Result: "42" (in lambda calculus)

## Examples:

See *submission/LambdaParser.hs* for few examples of usage.

## Documentation:

See *ProjectReport.pdf* for a documentation of the project.

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
