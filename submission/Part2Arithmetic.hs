module Part2Arithmetic where

import Parser
import Data.Builder

import Text.Read
import Data.Char

import Debug.Trace

-- TODO: temp memo
-- (+, -, *, **, ()) 
-- Order: () -> ** -> * -> - -> +

{-  BNF:

    <expr> ::= <operation> | <bracket> | <number>

    <bracket> ::= '(' <expr> ')'
    
    <operation> ::= <param> <op> <bexpr>
    
    <param> ::=  <parseBracket> | <number>

    <number> ::= <digit> <number> | <digit>

    <digit> ::= '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
    <op> ::= "+" | "-" | "**" | "*"

    Nota: For simplicity purpose, spaces have been omitted.
-}

---- [ Data structures ]

data Expr = Nb Double -- Number
            | Calc Op Expr Expr -- Operation
            | SubExpr Expr -- Bracket
            deriving Show

data Op = Plus | Minus | Power | Multi
    deriving (Show, Enum, Eq)

---- [ Expression evaluator ]

--- >>> resolve <$> parse parseExpression "9 * 2 - 3 - 4 * 2 + 1 + 2 - 1 * (1 + 2)"
--- Result >< Nb 7.0
---
resolve :: Expr -> Expr
resolve (SubExpr a) = resolve a
resolve expr = (resolveOperator Plus) 
                    $ (resolveOperator Minus) 
                    $ (resolveOperator Multi) 
                    $ resolveOperator Power expr

--- Resolve a specific calculation for the whole tree
resolveOperator :: Op -> Expr -> Expr
resolveOperator _ (Nb a) = Nb a
resolveOperator _ (SubExpr a) = resolve a
resolveOperator op (Calc operator a b) = if op == operator
    then resolveOperation op a b
    else Calc operator (propagate a) (propagate b)
    where
        propagate :: Expr -> Expr 
        propagate (SubExpr e) = resolve e
        propagate (Calc o x y) = resolveOperator op (Calc o x y)
        propagate x = id x -- do nothing

-- Resolve an operation (Calc)
resolveOperation :: Op -> Expr -> Expr -> Expr
resolveOperation op (Nb a) (Nb b) = Nb $ (getOperation op) a b
resolveOperation op (SubExpr e) b = resolveOperation op (resolve e) b
resolveOperation op a (SubExpr e) = resolveOperation op a (resolve e)
resolveOperation op (Nb a) (Calc o x y) = resolveOperator op $ Calc o (resolveOp x) (resolveOperator op y) 
    where 
        resolveOp (Nb n) = (Nb $ (getOperation op) a n)
        resolveOp (SubExpr e) = resolveOp (resolve e)
        resolveOp _ = error "this should not happen"
resolveOperation _ _ _ = error "this should not happen"

-- Get the corresponding function related to an Op
getOperation :: Op -> (Double -> Double -> Double)
getOperation Plus = (+)
getOperation Minus = (-)
getOperation Multi = (*)
getOperation Power = (**)

---- [ Operations ]

-- | x + y = add = λxy.y succ x
add :: Builder
add = lam 'x' $ lam 'y' $ (term 'y') `ap` succ `ap` (term 'x')

-- | x - y = minus = λxy.y pred x
minus :: Builder
minus = lam 'x' $ lam 'y' $ (term 'y') `ap` pred `ap` (term 'x')

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
-- | Note since we haven't encoded negative numbers pred 0 == 0, and m - n (where n > m) = 0

---- [ Parser ]

parseExpression :: Parser Expr
parseExpression = spaces *> -- skip spaces
    (parseOperation "-" 
    ||| parseOperation "+" 
    ||| parseOperation "**" 
    ||| parseOperation "*" 
    ||| parseBracket
    ||| parseNumber)

parseBracket :: Parser Expr
parseBracket = do
    b <- between (is '(') (is ')') parseExpression
    pure $ SubExpr b

parseParam :: Parser Expr
parseParam = spaces *>
    (parseBracket ||| parseNumber)

parseNumber :: Parser Expr
parseNumber = do
    _ <- spaces -- skip spaces
    str <- munch1 (isDigit)
    convertStr str
    where 
        convertStr :: String -> Parser Expr
        convertStr str = case (readMaybe str :: Maybe Double) of
            Just a -> pure (Nb a)
            Nothing -> Parser.fail $ ExpectedEof str

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