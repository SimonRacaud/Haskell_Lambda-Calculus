module LambdaParser where

import Parser
import Data.Lambda
import Data.Builder

import Part1 (lambda)
import Part2Logic (logicParser)
import Part2Arithmetic (arithmeticParser)
import Part2Complex (complexParser)
import Part3List (listParser, functionListParser)

-- You can add more imports if you need them

-- Remember that you can (and should) define your own functions, types, and
-- parser combinators. Each of the implementations for the functions below
-- should be fairly short and concise.


{-|
    Part 1
-}

-- | Exercise 1

-- | Parses a string representing a lambda calculus expression in long form
--
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
longLambdaP :: Parser Lambda
longLambdaP = build <$> (lambda True)

-- | Parses a string representing a lambda calculus expression in short form
--
-- >>> parse shortLambdaP "λxy.xy(xx)"
-- Result >< \xy.xy(xx)
--
-- >>> parse shortLambdaP "λx.x(λy.yy)"
-- Result >< \x.x\y.yy
--
-- >>> parse shortLambdaP "(λx.x)(λy.yy)"
-- Result >< (\x.x)\y.yy
--
-- >>> parse shortLambdaP "(λx.x)(λy.yy)λz.z"
-- Result >< (\x.x)(\y.yy)\z.z
--
-- >>> parse shortLambdaP "λxyz"
-- UnexpectedEof
--
-- >>> parse shortLambdaP "λx.xx"
-- Result >< \x.xx
--
-- >>> parse shortLambdaP "λλxy.(xx)xy"
-- UnexpectedChar '\955'
--
-- >>> parse shortLambdaP "(λx.x)λy.yy"
-- Result >< (\x.x)\y.yy
--
-- >>> parse shortLambdaP "(λx.x)(λy.y)(λx.xx)"
-- Result >< (\x.x)(\y.y)\x.xx
--
-- >>> parse shortLambdaP "λx.xλy.yλz.zz"
-- Result >< \x.x\y.y\z.zz
--
-- >>> parse shortLambdaP "λx.x(λy.y)(λz.zz)"
-- Result >< \x.x(\y.y)\z.zz
--
shortLambdaP :: Parser Lambda
shortLambdaP = build <$> (lambda False)

-- | Parses a string representing a lambda calculus expression in short or long form
-- parse lambdaP "xx"
-- UnexpectedChar 'x'
--
-- >>> parse shortLambdaP "λxyz"
-- UnexpectedEof
--
-- >>> parse lambdaP "λx.xx"
-- Result >< \x.xx
--
-- >>> parse lambdaP "(λx.xx)"
-- Result >< \x.xx
--
-- >>> parse lambdaP "λx..x"
-- UnexpectedChar '.'
--
-- >>> parse lambdaP "λx.A"
-- UnexpectedChar 'A'
--
-- >>> parse lambdaP "λxyz.xyz"
-- Result >< \xyz.xyz
--
-- >>> parse lambdaP "(λb.(λt.(λf.btf)))"
-- Result >< \btf.btf
--
-- >>> parse lambdaP "λxy.(xx)y"
-- Result >< \xy.xxy
--
-- >>> parse lambdaP "λxy.x(xy)"
-- Result >< \xy.x(xy)
--
-- >>> parse lambdaP "λb.λt.λf.btf"
-- Result >< \btf.btf
--
lambdaP :: Parser Lambda
lambdaP = longLambdaP ||| shortLambdaP  

{-|
    Part 2
-}

-- | Exercise 1

-- IMPORTANT: The church encoding for boolean constructs can be found here -> https://tgdwyer.github.io/lambdacalculus/#church-encodings

-- | Parse a logical expression and returns in lambda calculus
-- >>> lamToBool <$> parse logicP "True and False"
-- Result >< Just False
--
-- >>> lamToBool <$> parse logicP "True and False or not False and True"
-- Result >< Just True
--
-- >>> lamToBool <$> parse logicP "not not not False"
-- Result >< Just True
--
-- >>> parse logicP "True and False"
-- Result >< (\xy.(\btf.btf)xy\_f.f)(\t_.t)\_f.f
--
-- >>> parse logicP "not False"
-- Result >< (\x.(\btf.btf)x(\_f.f)\t_.t)\_f.f
--
-- >>> lamToBool <$> parse logicP "if True and not False then True or True else False"
-- Result >< Just True
--
-- >>> parse logicP "True and not not not False"
-- Result >< (\xy.(\btf.btf)xy\_f.f)(\t_.t)((\x.(\btf.btf)x(\_f.f)\t_.t)((\x.(\btf.btf)x(\_f.f)\t_.t)((\x.(\btf.btf)x(\_f.f)\t_.t)\_f.f)))
--
-- >>> lamToBool <$> parse logicP "True and not not not False"
-- Result >< Just True
--
-- >>> lamToBool <$> parse logicP "True and not not False"
-- Result >< Just False
--
-- >>> lamToBool <$> parse logicP "True or not not False"
-- Result >< Just True
--
-- >>> lamToBool <$> parse logicP "True or False and not not False or False"
-- Result >< Just True
--
-- >>> lamToBool <$> parse logicP "if True and not False then if True and not False then True or True else False else False"
-- Result >< Just True
--
-- >>> lamToBool <$> parse logicP "if True and not False then (if True and not False then True or True else False) else False"
-- Result >< Just True
--
-- >>> lamToBool <$> parse logicP "not  True and False or  True"
-- Result >< Just True
--
logicP :: Parser Lambda
logicP = build <$> logicParser

-- | Exercise 2

-- | The church encoding for arithmetic operations are given below (with x and y being church numerals)

-- | x + y = add = λxy.y succ x
-- | x - y = minus = λxy.y pred x
-- | x * y = multiply = λxyf.x(yf)
-- | x ** y = exp = λxy.yx

-- | The helper functions you'll need are:
-- | succ = λnfx.f(nfx)
-- | pred = λnfx.n(λgh.h(gf))(λu.x)(λu.u)
-- | Note since we haven't encoded negative numbers pred 0 == 0, and m - n (where n > m) = 0

-- | Parse simple arithmetic expressions involving + - and natural numbers into lambda calculus
-- >>> lamToInt <$> parse basicArithmeticP "5 + 4"
-- Result >< Just 9
--
-- >>> lamToInt <$> parse basicArithmeticP "5 + 9 - 3 + 2"
-- Result >< Just 13
basicArithmeticP :: Parser Lambda
basicArithmeticP = arithmeticParser

-- | Parse arithmetic expressions involving + - * ** () and natural numbers into lambda calculus
-- >>> lamToInt <$> parse arithmeticP "5 + 9 * 3 - 2**3"
-- Result >< Just 24
--
-- >>> lamToInt <$> parse arithmeticP "100 - 4 * 2**(4-1)"
-- Result >< Just 68
--
-- >>> lamToInt <$> parse arithmeticP "1 + 8 * 8 + 2 * 8"
-- Result >< Just 81
--
-- >>> lamToInt <$> parse arithmeticP "1 - 2"
-- Result >< Just 0
--
-- >>> lamToInt <$> parse arithmeticP "2**8"
-- Result >< Just 256
--
-- >>> lamToInt <$> parse arithmeticP "2 **8 - 1"
-- Result >< Just 255
--
-- >>> lamToInt <$> parse arithmeticP "2 **(8 - 1)"
-- Result >< Just 128
--
-- >>> lamToInt <$> parse arithmeticP "2* 2 **(8 - 1)"
-- Result >< Just 256
--
-- >>> parse arithmeticP "/ 2* 2 ** /"
-- UnexpectedChar '/'
--
arithmeticP :: Parser Lambda
arithmeticP = arithmeticParser

-- | Exercise 3

-- | The church encoding for comparison operations are given below (with x and y being church numerals)

-- | x <= y = LEQ = λmn.isZero (minus m n)
-- | x == y = EQ = λmn.and (LEQ m n) (LEQ n m)

-- | The helper function you'll need is:
-- | isZero = λn.n(λx.False)True

-- |
-- >>> lamToBool <$> parse complexCalcP "9 - 2 <= 3 + 6"
-- Result >< Just True
--
-- >>> lamToBool <$> parse complexCalcP "15 - 2 * 2 != 2**3 + 3 or 5 * 3 + 1 < 9"
-- Result >< Just False
--
-- >>> lamToBool <$> parse complexCalcP "not not True"
-- Result >< Just True
--
-- >>> lamToBool <$> parse complexCalcP "not not not True"
-- Result >< Just False
--
-- >>> lamToBool <$> parse complexCalcP "not not not True and True"
-- Result >< Just False
--
-- >>> lamToBool <$> parse complexCalcP "if True or False then False else True"
-- Result >< Just False
--
-- >>> lamToBool <$> parse complexCalcP "1 > 2"
-- Result >< Just False
--
-- >>> lamToBool <$> parse complexCalcP "3 > 2"
-- Result >< Just True
--
-- >>> lamToBool <$> parse complexCalcP "2 > 2"
-- Result >< Just False
--
-- >>> lamToBool <$> parse complexCalcP "1 < 2"
-- Result >< Just True
--
-- >>> lamToBool <$> parse complexCalcP "2 < 2"
-- Result >< Just False
--
-- >>> lamToBool <$> parse complexCalcP "3 < 2"
-- Result >< Just False
--
-- >>> lamToBool <$> parse complexCalcP "1 == 2"
-- Result >< Just False
--
-- >>> lamToBool <$> parse complexCalcP "1 == 1"
-- Result >< Just True
--
-- >>> lamToBool <$> parse complexCalcP "1 != 2"
-- Result >< Just True
--
-- >>> lamToBool <$> parse complexCalcP "1 != 1"
-- Result >< Just False
--
complexCalcP :: Parser Lambda
complexCalcP = complexParser

{-|
    Part 3
-}

-- | Exercise 1

-- | The church encoding for list constructs are given below
-- | [] = null = λcn.n
-- | isNull = λl.l(λht.False) True
-- | cons = λhtcn.ch(tcn)
-- | head = λl.l(λht.h) False
-- | tail = λlcn.l(λhtg.gh(tc))(λt.n)(λht.t)
--
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
--
-- >>> parse listP "[True, False, True, False, False]"
-- Result >< (\htcn.ch(tcn))(\xy.x)((\htcn.ch(tcn))(\xy.y)((\htcn.ch(tcn))(\xy.x)((\htcn.ch(tcn))(\xy.y)((\htcn.ch(tcn))(\xy.y)\cn.n))))
--
-- >>> parse listP "[0, [1, [2]]]"
-- Result >< (\htcn.ch(tcn))(\fx.x)((\htcn.ch(tcn))((\htcn.ch(tcn))(\f.f)((\htcn.ch(tcn))((\htcn.ch(tcn))(\fx.f(fx))(\cn.n))(\cn.n)))\cn.n)
--
listP :: Parser Lambda
listP = build <$> listParser

-- | 
-- >>> lamToBool <$> parse listOpP "head [True, False, True, False, False]"
-- Result >< Just True
--
-- >>> lamToBool <$> parse listOpP "head rest [True, False, True, False, False]"
-- Result >< Just False
--
-- >>> lamToBool <$> parse listOpP "isNull []"
-- Result >< Just True
--
-- >>> lamToBool <$> parse listOpP "isNull [1, 2, 3]"
-- Result >< Just False
--
--
-- >>> lamToInt <$> parse listOpP "head rest [42, 43, 44, 45]"
-- Result >< Just 43
--
-- >>> lamToInt <$> parse listOpP "head [4, 3, 2]"
-- Result >< Just 4
--
-- >>> lamToBool <$> parse listOpP "isNull null"
-- Result >< Just True
--
-- >>> lamToBool <$> parse listOpP "isNull tail [1, 2, 3]"
-- Result >< Just False
--
-- >>> lamToBool <$> parse listOpP "isNull tail [1]"
-- Result >< Just True
--
-- >>> lamToBool <$> parse listOpP "head cons True null"
-- Result >< Just True
--
-- >>> lamToInt <$> parse listOpP "head cons 5 null"
-- Result >< Just 5
--
-- >>> lamToInt <$> parse listOpP "head cons 5 [3, 4, 6]"
-- Result >< Just 5
--
-- >>> lamToInt <$> parse listOpP "head tail cons 5 [3, 4, 6]"
-- Result >< Just 3
--
-- >>> lamToBool <$> parse listOpP "head [True and False, True, False]"
-- Result >< Just False
--
-- >>> lamToInt <$> parse listOpP "head [10 - 4 * 2]"
-- Result >< Just 2
--
-- >>> lamToBool <$> parse listOpP "head [10 - 4 * 2 > 1]"
-- Result >< Just True
--
-- >>> lamToBool <$> parse listOpP "head [(λxy.x)]"
-- Result >< Just True
--
-- >>> lamToBool <$> parse listOpP "head [(λxy.y)]"
-- Result >< Just False
--
listOpP :: Parser Lambda
listOpP = functionListParser ||| listP

-- | Exercise 2

-- | Implement your function(s) of choice below!
