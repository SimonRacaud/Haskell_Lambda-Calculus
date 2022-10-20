module Part2Arithmetic where

import Parser
import Data.Lambda
import Data.Builder

import Text.Read
import Data.Char

{-  BNF:

    <expr> ::= <operation> | <bracket> | <number>
    <bracket> ::= "(" <expr> ")"    
    <operation> ::= <param> <op> <expr>
    <param> ::=  <bracket> | <number>
    <number> ::= <digit> <number> | <digit>
    <digit> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
    <op> ::= "+" | "-" | "**" | "*"

    Nota: For simplicity purpose, spaces have been omitted.
-}

---- [ Data structures ]

-- Expression tree structure
data Expr = Nb Builder -- Number (lambda)
            | Calc Op Expr Expr -- Operation
            | SubExpr Expr -- Bracket
            deriving Show

-- Operators enum
data Op = Plus | Minus | Power | Multi
    deriving (Show, Enum, Eq)

---- [ Main function  ]

-- | Resolve an arithmetic operation
-- >>> lamToInt <$> parse arithmeticParser "9 * 2 + 3 + 4 * 2 + 1 + 2 + 1 * (1 + 2)"
-- Result >< Just 35
--
arithmeticParser :: Parser Lambda
arithmeticParser = do
    tree <- parseExpression -- Parse string and generate a tree of expression
    execResolver tree -- Resolve tree to one lambda Builder
    where
        execResolver :: Expr -> Parser Lambda
        execResolver tree = case resolve tree of
            (Nb value) -> pure $ build $ value
            _ -> Parser.fail UnexpectedEof -- Fail to resolve expression

---- [ Expression evaluator ]

-- Resolve any expression
resolve :: Expr -> Expr
resolve (SubExpr a) = resolve a -- resolve sub-expression
    -- Resolve operations by priority order
resolve expr = (resolveOperator Plus) 
                    $ (resolveOperator Minus) 
                    $ (resolveOperator Multi) 
                    $ resolveOperator Power expr

--- Resolve a specific operation for the whole tree
resolveOperator :: Op -> Expr -> Expr
resolveOperator _ (Nb a) = Nb a
resolveOperator _ (SubExpr a) = resolve a
resolveOperator op (Calc operator a b) = if op == operator
    then resolveOperation op a b
    else Calc operator (propagate a) (propagate b)
    where
        -- Propagate the resolver in the tree
        propagate :: Expr -> Expr 
        propagate (SubExpr e) = resolve e
        propagate (Calc o x y) = resolveOperator op (Calc o x y)
        propagate x = id x -- do nothing

-- Resolve an operation (Calc)
resolveOperation :: Op -> Expr -> Expr -> Expr
resolveOperation op (Nb a) (Nb b) = Nb $ solver op a b -- < Resolve operation
resolveOperation op (SubExpr e) b = resolveOperation op (resolve e) b
resolveOperation op a (SubExpr e) = resolveOperation op a (resolve e)
resolveOperation op (Nb a) (Calc o x y) = resolveOperator op $ Calc o (resolveOp x) (resolveOperator op y) 
    where 
        resolveOp (Nb n) = (Nb $ solver op a n) -- < Resolve operation
        resolveOp (SubExpr e) = resolveOp (resolve e)
        resolveOp _ = error "this should not happen"
resolveOperation _ _ _ = error "this should not happen"

-- Combine two lambda with operation
solver :: Op -> Builder -> Builder -> Builder
solver Plus a b = add `ap` a `ap` b
solver Minus a b = minus `ap` a `ap` b
solver Multi a b = multiply `ap` a `ap` b
solver Power a b = Part2Arithmetic.exp `ap` a `ap` b

---- [ Lambda Operations ]

-- | x + y = add = λxy.y succ x
add :: Builder
add = lam 'x' $ lam 'y' $ (term 'y') `ap` Part2Arithmetic.succ `ap` (term 'x')

-- | x - y = minus = λxy.y pred x
minus :: Builder
minus = lam 'x' $ lam 'y' $ (term 'y') `ap` Part2Arithmetic.pred `ap` (term 'x')

-- | x * y = multiply = λxyf.x(yf)
multiply :: Builder
multiply = lam 'x' $ lam 'y' $ lam 'f' $ (term 'x') `ap` ((term 'y') `ap` (term 'f'))

-- | x ** y = exp = λxy.yx
exp :: Builder
exp = lam 'x' $ lam 'y' $ (term 'y') `ap` (term 'x')

-- | succ = λnfx.f(nfx)
succ :: Builder
succ = lam 'n' $ lam 'f' $ lam 'x' $ (term 'f') `ap` ((term 'n') `ap` (term 'f') `ap` (term 'x'))
 
-- | pred = λnfx.n(λgh.h(gf))(λu.x)(λu.u)
pred :: Builder
pred = lam 'n' $ lam 'f' $ lam 'x' $ (term 'n') 
                `ap` (lam 'g' $ lam 'h' $ (term 'h') `ap` ((term 'g') `ap` (term 'f')))
                `ap` (lam 'u' (term 'x'))
                `ap` (lam 'u' (term 'u'))
-- Note: since we haven't encoded negative numbers pred 0 == 0, and m - n (where n > m) = 0

---- [ Parser ]

-- Parse an expression
parseExpression :: Parser Expr
parseExpression = spaces *> -- skip spaces
    (parseOperation "-"  -- Try each operator
    ||| parseOperation "+" 
    ||| parseOperation "**" 
    ||| parseOperation "*" 
    ||| parseBracket
    ||| parseNumber)

-- Parse sub-expression
parseBracket :: Parser Expr
parseBracket = do
    b <- bracket parseExpression
    pure $ SubExpr b

-- Parse operation parameter
parseParam :: Parser Expr
parseParam = spaces *>
    (parseBracket ||| parseNumber)

-- Parse number
parseNumber :: Parser Expr
parseNumber = do
    _ <- spaces -- skip spaces
    str <- munch1 (isDigit)
    convertStr str
    where 
        convertStr :: String -> Parser Expr
        convertStr str = case (readMaybe str :: Maybe Int) of
            Just a -> pure (Nb $ intToLam a)
            Nothing -> Parser.fail $ ExpectedEof str

-- Parse one operation
-- The symbol of the operator to parse is given as parameter (e.g. "+")
parseOperation :: String -> Parser Expr
parseOperation s = do
    a <- parseParam
    _ <- spaces -- skip spaces
    _ <- string s
    b <- parseExpression
    pure $ Calc (getOp s) a b
    where 
        getOp :: String -> Op
        getOp str = case str of
            "+" -> Plus
            "-" -> Minus
            "**" -> Power
            "*" -> Multi
            _ -> error "Internal error" -- programming error