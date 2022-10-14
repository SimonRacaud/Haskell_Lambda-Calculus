module Part2Complex where

import Parser
import Data.Lambda
import Data.Builder

import Part2Arithmetic (minus, add, multiply, exp)
import Part2Logic (genAnd, genFalse, genTrue, genNot, genOr)

import Data.Char
import Text.Read
import Data.List

{- BNF
    <stmt> ::= <ifCond> |  "(" <ifCond> ")" | <logicExpr> | <arithmExpr>
    <ifCond> ::= "if" <spaces> <logicExpr> <spaces> "then" <spaces> <stmt> <spaces> "else" <spaces> <stmt>

    <logicExpr> ::= <logicDuop> | <logicUnop> | "(" <logicExpr> ")" | <logicParam>
    <logicDuop> ::= <logicPrevParam> <spaces> <duop> <spaces> <logicExpr>
    <logicUnop> ::= <unop> <spaces> <logicParam>
    <logicPrevParam> ::=  "(" <logicExpr> ")" | <logicParam>
    <logicParam> ::= <bool> | <comparisonExpr>

    <comparisonExpr> ::= <comparisonOperation> 
    <comparisonOperation> ::= <comparisonPrevParam> <spaces> <cmpOp> <spaces> <comparisonParam> 
    <comparisonPrevParam> ::= "(" <comparisonExpr> ")" | <number> | <arithmExpr>
    <comparisonParam> ::= <comparisonExpr> | <number> | <arithmExpr>

    <arithmExpr> ::= <operation> | <number> | "(" <arithmExpr> ")"
    <operation> ::= <arithmPrevParam> <spaces> <arithmOp> <spaces> <arithmExpr>
    <arithmPrevParam> ::= "(" <arithmExpr> ")" | <number>

    <number> ::= <digit> <number> | <digit>
    <digit> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
    <bool> ::= "True" | "False"

    <arithmOp> ::= "+" | "-" | "**" | "*"
    <cmpOp> ::= ">=" | "<=" | "==" | "!=" | "<" | ">"
    <unop> ::= "not"
    <duop> ::= "and" | "or"

    <spaces> ::= " " <spaces> | " "

-}

---- [ Data structures ]

data Stmt = Expr Expr
            | If Expr Stmt Stmt   
            deriving Show

data Expr = Var Builder
            | Uno Op Expr -- Only for NOT
            | Duo Op Expr Expr
            | SubExpr Expr -- Brackets () or <comparisonExpr> or <arithmExpr>
            deriving Show

data Op = PLUS | MINUS | POW | MULT | EQU | NEQ | LEQ | UEQ | LOW | UP | AND | OR | NOT
    deriving (Show, Enum, Eq)

data Operator = Operator String Op

---- [ Operator management ]

arithmOperators :: [Operator]
arithmOperators = [ Operator "+" PLUS
                  , Operator "-" MINUS
                  , Operator "**" POW
                  , Operator "*" MULT ]

compOperators :: [Operator]
compOperators = [ Operator "==" EQU
                , Operator "!=" NEQ 
                , Operator "<=" LEQ
                , Operator ">=" UEQ
                , Operator ">" UP
                , Operator "<" LOW
                ]

logicDuoOperators :: [Operator]
logicDuoOperators = [ Operator "and" AND
                 , Operator "or" OR ]

logicUnoOperators :: [Operator]
logicUnoOperators = [ Operator "not" NOT ]

operatorStr :: Operator -> String
operatorStr (Operator str _) = str

operatorOp :: Operator -> Op
operatorOp (Operator _ op) = op

strToOp :: String -> Op
strToOp str = case tryEach [arithmOperators, compOperators, logicDuoOperators, logicUnoOperators] of
        (Just op) -> op
        Nothing -> error "The operator doesn't exist." -- this should never happen
    where
        tryEach :: [[Operator]] -> Maybe Op
        tryEach (x:xs) = case findOp x of
            (Just op) -> Just op
            Nothing -> tryEach xs 
        tryEach [] = Nothing

        findOp :: [Operator] -> Maybe Op
        findOp listOp = case find (((==) str).operatorStr) listOp of
            (Just a) -> Just $ operatorOp a
            Nothing -> Nothing

---- [ Main function  ]

-- complexParser :: Parser Lambda
-- complexParser = do
--     tree <- parseExpression
--     case resolve tree of
--         (Nb value) -> pure $ build $ value
--         _ -> Parser.fail UnexpectedEof -- Fail to resolve expression

---- [ Expression evaluator ]

-- resolve :: Expr -> Expr
-- resolve (SubExpr a) = resolve a
-- resolve expr = (resolveOperator UP)
--             $ (resolveOperator LOW)
--             $ (resolveOperator EQU)
--             $ (resolveOperator NEQ)
--             $ (resolveOperator LEQ) 
--             $ (resolveOperator UEQ) 
--             $ (resolveOperator PLUS) 
--             $ (resolveOperator MINUS) 
--             $ (resolveOperator MULT) 
--             $ resolveOperator POW expr

-- --- Resolve a specific calculation for the whole tree
-- resolveOperator :: Op -> Expr -> Expr
-- resolveOperator _ (Nb a) = Nb a
-- resolveOperator _ (SubExpr a) = resolve a
-- resolveOperator op (Calc operator a b) = if op == operator
--     then resolveOperation op a b
--     else Calc operator (propagate a) (propagate b)
--     where
--         propagate :: Expr -> Expr 
--         propagate (SubExpr e) = resolve e
--         propagate (Calc o x y) = resolveOperator op (Calc o x y)
--         propagate x = id x -- do nothing

-- -- Resolve an operation (Calc)
-- resolveOperation :: Op -> Expr -> Expr -> Expr
-- resolveOperation op (Nb a) (Nb b) = Nb $ solver op a b -- < Resolve operation
-- resolveOperation op (SubExpr e) b = resolveOperation op (resolve e) b
-- resolveOperation op a (SubExpr e) = resolveOperation op a (resolve e)
-- resolveOperation op (Nb a) (Calc o x y) = resolveOperator op $ Calc o (resolveOp x) (resolveOperator op y) 
--     where 
--         resolveOp (Nb n) = (Nb $ solver op a n) -- < Resolve operation
--         resolveOp (SubExpr e) = resolveOp (resolve e)
--         resolveOp _ = error "this should not happen"
-- resolveOperation _ _ _ = error "this should not happen"

-- Solve an operation between two lambda
-- solver :: Op -> Builder -> Builder -> Builder
-- solver PLUS a b = add `ap` a `ap` b
-- solver MINUS a b = minus `ap` a `ap` b
-- solver MULT a b = multiply `ap` a `ap` b
-- solver POW a b = Part2Arithmetic.exp `ap` a `ap` b
-- solver EQU a b = eq `ap` a `ap` b
-- solver NEQ a b = neq `ap` a `ap` b
-- solver LEQ a b = leq `ap` a `ap` b
-- solver UEQ a b = ueq `ap` a `ap` b
-- solver LOW a b = lower `ap` a `ap` b
-- solver UP a b = upper `ap` a `ap` b

---- [ Comparison Operations ]

-- | x <= y = LEQ = λmn.isZero (minus m n)
leq :: Builder
leq = lam 'm' $ lam 'n' $ isZero `ap` (minus `ap` (term 'm') `ap` (term 'n'))

ueq :: Builder -- (<=)
ueq = genOr `ap` upper `ap` eq

upper :: Builder -- (>)
upper = genNot `ap` leq

lower :: Builder -- (<)
lower = genAnd `ap` leq `ap` (genNot `ap` eq)

-- | x == y = EQ = λmn.and (LEQ m n) (LEQ n m)
eq :: Builder
eq = lam 'm' $ lam 'n' $ (genAnd 
                            `ap` (leq `ap` (term 'm') `ap` (term 'n')) 
                            `ap` (leq `ap` (term 'n') `ap` (term 'm')))

neq :: Builder -- (!=)
neq = genNot `ap` eq

-- | isZero = λn.n(λx.False)True
isZero :: Builder
isZero = lam 'n' $ (term 'n') `ap` (lam 'x' genFalse) `ap` genTrue

------ [ Parser ]

--- [ Logical layer ]

stmtParser :: Parser Stmt
stmtParser = spaces *>
    (ifCondParser 
    ||| (bracket ifCondParser) 
    ||| (Expr <$> logicExprParser)
    ||| (Expr <$> arithmExprParser))

ifCondParser :: Parser Stmt
ifCondParser = do
    _ <- stringTok "if"
    cond <- logicExprParser
    _ <- stringTok "then"
    positif <- stmtParser
    _ <- stringTok "else"
    negatif <- stmtParser
    pure $ If cond positif negatif

logicExprParser :: Parser Expr
logicExprParser = logicDuopParser 
                        ||| logicUnopParser 
                        ||| subExprParser logicExprParser 
                        ||| logicParamParser

logicDuopParser :: Parser Expr
logicDuopParser = parseEachOperator logicDuoOperators duopParser 
    where
        duopParser :: String -> Parser Expr
        duopParser = duoOperationParser logicPrevParamParser logicExprParser

logicUnopParser :: Parser Expr
logicUnopParser = parseEachOperator logicUnoOperators $ unoOperationParser logicParamParser

logicPrevParamParser :: Parser Expr
logicPrevParamParser = subExprParser logicExprParser ||| logicParamParser

logicParamParser :: Parser Expr
logicParamParser = boolParser ||| compExprParser

boolParser :: Parser Expr
boolParser = (stringTok "True" *> (pure $ Var genTrue)) 
                ||| (stringTok "False" *> (pure $ Var genFalse)) 
            <* spaces

--- [ Comparison Layer ]

compExprParser :: Parser Expr
compExprParser = parseEachOperator compOperators compOperationParser
    where 
        compOperationParser :: String -> Parser Expr
        compOperationParser = duoOperationParser compPrevParamParser compParamParser

compPrevParamParser :: Parser Expr
compPrevParamParser = subExprParser compExprParser ||| arithmExprParser ||| numberParser

compParamParser :: Parser Expr
compParamParser = compExprParser ||| arithmExprParser ||| numberParser

--- [ Arithmetic Layer ]

arithmExprParser :: Parser Expr
arithmExprParser = operationParser ||| numberParser ||| subExprParser arithmExprParser

operationParser :: Parser Expr
operationParser = parseEachOperator arithmOperators parseOneOp 
    where 
        parseOneOp :: String -> Parser Expr
        parseOneOp = duoOperationParser arithmPrevParamParser arithmExprParser

arithmPrevParamParser :: Parser Expr
arithmPrevParamParser = subExprParser arithmExprParser ||| numberParser

--- [ Common functions ]

duoOperationParser :: Parser Expr -> Parser Expr -> String -> Parser Expr
duoOperationParser prevParam postParam op = do
    x <- prevParam -- first parameter
    _ <- stringTok op -- operator
    y <- postParam -- second parameter
    pure $ Duo (strToOp op) x y

unoOperationParser :: Parser Expr -> String -> Parser Expr
unoOperationParser paramParser op = do
    _ <- stringTok op
    a <- paramParser
    pure $ Uno (strToOp op) a

-- Parse the content of an expression under two brackets using the parser (a)
--  and return the result in a SubExpr object
subExprParser :: Parser Expr -> Parser Expr
subExprParser a = SubExpr <$> (bracket a) <* spaces

numberParser :: Parser Expr
numberParser = do
    nb <- int
    _ <- spaces -- skip spaces
    pure $ Var $ intToLam nb

-- Using a log of operator and a parser given as parameter, 
--    call the parser for each operator in the array until the operation succeed
parseEachOperator :: [Operator] -> (String -> Parser Expr) -> Parser Expr
parseEachOperator operatorList parser = parseEach operatorList $ \op -> parser $ operatorStr op
