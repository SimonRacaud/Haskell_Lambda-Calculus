module Part2Complex where

import Parser hiding (lower)
import Data.Lambda
import Data.Builder

import qualified Part2Arithmetic as Arithm
import qualified Part2Logic as Logic

import Data.List

import Debug.Trace (trace)

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

-- Logical Statement structure
data Stmt = Expr Expr -- Expression Tree
            | If Expr Stmt Stmt -- If statement
            deriving Show

-- Tree of expression (logical, arithmetic or comparison)
data Expr = Var Builder -- Single value (Lambda builder)
            | Uno Op Expr -- Unary operation (Not)
            | Duo Op Expr Expr -- Dual operations
            | SubExpr Expr -- Sub-expression
            deriving Show

-- Operators enum
data Op = PLUS | MINUS | POW | MULT | EQU | NEQ | LEQ | UEQ | LOW | UP | AND | OR | NOT
    deriving (Show, Enum, Eq)

-- Operator structure => Operator symbol + corresponding enum
data Operator = Operator String Op

---- [ Operator management ]

-- List of arithmetic operators
arithmOperators :: [Operator]
arithmOperators = [ Operator "+" PLUS
                  , Operator "-" MINUS
                  , Operator "**" POW
                  , Operator "*" MULT ]

-- List of comparison operators
compOperators :: [Operator]
compOperators = [ Operator "==" EQU
                , Operator "!=" NEQ 
                , Operator "<=" LEQ
                , Operator ">=" UEQ
                , Operator ">" UP
                , Operator "<" LOW
                ]

-- List of logical dual operators
logicDuoOperators :: [Operator]
logicDuoOperators = [ Operator "and" AND
                 , Operator "or" OR ]

-- List of logical unary operators
logicUnoOperators :: [Operator]
logicUnoOperators = [ Operator "not" NOT ]

-- Extract string symbol
operatorStr :: Operator -> String
operatorStr (Operator str _) = str

-- Extract operator enum
operatorOp :: Operator -> Op
operatorOp (Operator _ op) = op

-- Convert operator symbol to Enum
strToOp :: String -> Op
strToOp str = case findOp (arithmOperators ++ compOperators ++ logicDuoOperators ++ logicUnoOperators) of
        (Just op) -> op
        Nothing -> error "The operator doesn't exist." -- this should never happen
    where
        findOp :: [Operator] -> Maybe Op
        findOp listOp = case find (((==) str).operatorStr) listOp of
            (Just a) -> Just $ operatorOp a
            Nothing -> Nothing

---- [ Main function  ]

-- Parse a complex expression string to a Lambda
complexParser :: Parser Lambda
complexParser = do
    tree <- stmtParser -- Parse string to an expression tree
    build <$> resolveStmt tree -- Reduce the tree to one lambda expression


---- [ Expression evaluator ]

--- [ Logic layer ]

-- Error trace
traceExpr :: Expr -> (a -> a)
traceExpr expr = trace ("Fail to resolve expression: " ++ show expr)

-- Resolve statement
resolveStmt :: Stmt -> Parser Builder
resolveStmt (Expr e) = toBuilder $ resolve e -- Resolve expression
    where 
        toBuilder (Var v) = pure $ v
        toBuilder expr = traceExpr expr $ Parser.fail UnexpectedEof -- Fail to resolve expression
resolveStmt (If c a b) = resolveIfCond c a b -- Resolve If statement

-- Resolve 'If then else' condition
resolveIfCond :: Expr -> Stmt -> Stmt -> Parser Builder
resolveIfCond c a b = do
        cond <- (getVar $ resolve c) -- Condition
        pos <- resolveStmt a         -- Then expression
        neg <- resolveStmt b         -- Else expression 
        pure $ ((Logic.genIf `ap` cond) `ap` pos) `ap` neg -- Combine result
    where
        -- Extract result of resolution or return an error
        getVar :: Expr -> Parser Builder 
        getVar (Var v) = pure $ v
        getVar e = traceExpr e $ Parser.fail UnexpectedEof -- Fail to resolve expression
    
--- [ Comparison & Arithmetic Layers ]

-- Resolve operations by order of priority
resolve :: Expr -> Expr
resolve (SubExpr a) = resolve a
resolve expr = (resolveOperator OR)
            $ (resolveOperator AND)
            $ (resolveOperator NOT)
            $ (resolveOperator UP)
            $ (resolveOperator LOW)
            $ (resolveOperator EQU)
            $ (resolveOperator NEQ)
            $ (resolveOperator LEQ) 
            $ (resolveOperator UEQ) 
            $ (resolveOperator PLUS) 
            $ (resolveOperator MINUS) 
            $ (resolveOperator MULT) 
            $ resolveOperator POW expr

--- Resolve any specific operation 
resolveOperator :: Op -> Expr -> Expr
resolveOperator _ (Var a) = Var a
resolveOperator _ (SubExpr a) = resolve a
resolveOperator op (Duo operator a b) = if op == operator
    then resolveDuop op a b
    else Duo operator (propagate op a) (propagate op b)    
resolveOperator op (Uno operator a) = if op == operator
    then resolveUnop op a
    else Uno operator (propagate op a)

-- Propagate resolver to sub-operations and expressions
propagate :: Op -> Expr -> Expr 
propagate _ (SubExpr e) = resolve e
propagate op (Duo o x y) = resolveOperator op (Duo o x y)
propagate op (Uno o x) = resolveOperator op (Uno o x)
propagate _ x = id x -- do nothing

-- Resolve an operation (Dual)
resolveDuop :: Op -> Expr -> Expr -> Expr
resolveDuop op (Var a) (Var b) = Var $ solverDuop op a b -- Resolve operation
resolveDuop op (SubExpr e) b = resolveDuop op (resolve e) b -- solve sub-expr
resolveDuop op a (SubExpr e) = resolveDuop op a (resolve e) -- solve sub-expr
    -- Resolve dual operation:
resolveDuop op (Var a) (Duo o x y) = 
    resolveOperator op $ Duo o (resolveOp x) (resolveOperator op y) 
    where 
        -- Solve first parameter of a Duo (either a Var or a SubExpr):
        resolveOp :: Expr -> Expr
        resolveOp (Var n) = (Var $ solverDuop op a n) -- < Resolve operation
        resolveOp (SubExpr e) = resolveOp (resolve e)
        resolveOp _ = error "this should never happen"
resolveDuop _ _ _ = error "this should never happen"

-- Resolve an operation (Unary)
resolveUnop :: Op -> Expr -> Expr
resolveUnop op (Var a) = Var $ solverUnop op a -- Resolve operation
resolveUnop op (SubExpr e) = resolveUnop op $ resolve e
resolveUnop op (Duo o a b) = Duo o (resolveUnop op a) (resolveOperator op b)
resolveUnop op (Uno o a) = resolveUnop op $ resolveUnop o a

-- Solve an operation between two lambda
solverDuop :: Op -> Builder -> Builder -> Builder
solverDuop PLUS a b = Arithm.add `ap` a `ap` b
solverDuop MINUS a b = Arithm.minus `ap` a `ap` b
solverDuop MULT a b = Arithm.multiply `ap` a `ap` b
solverDuop POW a b = Arithm.exp `ap` a `ap` b
solverDuop EQU a b = eq `ap` a `ap` b
solverDuop NEQ a b = neq `ap` a `ap` b
solverDuop LEQ a b = leq `ap` a `ap` b
solverDuop UEQ a b = ueq `ap` a `ap` b
solverDuop LOW a b = lower `ap` a `ap` b
solverDuop UP a b = greater `ap` a `ap` b
solverDuop AND a b = Logic.genAnd `ap` a `ap` b
solverDuop OR a b = Logic.genOr `ap` a `ap` b
solverDuop _ _ _ = error "Unexpected error"

-- Solve an operation on one lambda
solverUnop :: Op -> Builder -> Builder
solverUnop NOT a = Logic.genNot `ap` a
solverUnop _ _ = error "Unexpected error"

---- [ Comparison Operations ]

-- | x <= y = LEQ = λmn.isZero (minus m n)
leq :: Builder
leq = lam 'm' $ lam 'n' $ isZero `ap` (Arithm.minus `ap` (term 'm') `ap` (term 'n'))

ueq :: Builder -- (a <= b) is the same then (b >= a)
ueq = lam 'm' $ lam 'n' $ leq `ap` (term 'n') `ap` (term 'm')

greater :: Builder -- (>) which is (not <=)
greater = lam 'm' $ lam 'n' $ Logic.genNot `ap` (leq `ap` (term 'm') `ap` (term 'n'))

lower :: Builder -- (<) which is (not >=)
lower =  lam 'm' $ lam 'n' $ Logic.genNot `ap` (ueq `ap` (term 'm') `ap` (term 'n'))

-- | x == y = EQ = λmn.and (LEQ m n) (LEQ n m)
eq :: Builder
eq = lam 'm' $ lam 'n' $ (Logic.genAnd 
                            `ap` (leq `ap` (term 'm') `ap` (term 'n')) 
                            `ap` (leq `ap` (term 'n') `ap` (term 'm')))

neq :: Builder -- (!=) is literally (not ==)
neq = lam 'm' $ lam 'n' $ Logic.genNot `ap` (eq `ap` (term 'm') `ap` (term 'n'))

-- | isZero = λn.n(λx.False)True
isZero :: Builder
isZero = lam 'n' $ (term 'n') `ap` (lam 'x' genFalse) `ap` genTrue

-- true = \xy.x
genTrue :: Builder
genTrue = lam 'x' $ lam 'y' (term 'x')

-- false = \xy.y
genFalse :: Builder
genFalse = lam 'x' $ lam 'y' (term 'y')


------ [ Parser ]

--- [ Logical layer ]

-- Parse statement
stmtParser :: Parser Stmt
stmtParser = spaces *>
    (ifCondParser -- If condition
    ||| (bracket ifCondParser) -- Sub-expression with if condition
    ||| (Expr <$> logicExprParser) -- Logical expression
    ||| (Expr <$> arithmExprParser)) -- Arithmetic expression

-- Parse If statement
ifCondParser :: Parser Stmt
ifCondParser = spaces *> do
    _ <- stringTok "if"
    cond <- logicExprParser
    _ <- stringTok "then"
    positif <- stmtParser
    _ <- stringTok "else"
    negatif <- stmtParser
    pure $ If cond positif negatif

-- Parse Logical expression
logicExprParser :: Parser Expr
logicExprParser = logicDuopParser -- Dual operation
                    ||| logicUnopParser  -- Unary operation
                    ||| subExprParser logicExprParser  -- Sub-expression "()"
                    ||| logicParamParser -- Boolean or arithmetic operation

-- Parse Dual logical operation
logicDuopParser :: Parser Expr
logicDuopParser = parseEachOperator logicDuoOperators duopParser 
    where
        duopParser :: String -> Parser Expr
        duopParser = duoOperationParser logicPrevParamParser logicExprParser

-- Parse Unary logical operation
logicUnopParser :: Parser Expr
logicUnopParser = parseEachOperator logicUnoOperators $ unoOperationParser logicExprParser

-- Parse first parameter of logical dual operation
logicPrevParamParser :: Parser Expr
logicPrevParamParser = subExprParser logicExprParser ||| logicParamParser

-- Parse logical expression parameter
logicParamParser :: Parser Expr
logicParamParser = boolParser ||| compExprParser

-- Parse boolean
boolParser :: Parser Expr
boolParser = (stringTok "True" *> (pure $ Var genTrue)) 
                ||| (stringTok "False" *> (pure $ Var genFalse)) 
            <* spaces

--- [ Comparison Layer ]

-- Parse comparison expression
compExprParser :: Parser Expr
compExprParser = parseEachOperator compOperators compOperationParser
    where 
        compOperationParser :: String -> Parser Expr
        compOperationParser = duoOperationParser compPrevParamParser compParamParser

-- Parse comparison operation first parameter
compPrevParamParser :: Parser Expr
compPrevParamParser = subExprParser compExprParser ||| arithmExprParser ||| numberParser

-- Parse comparison expression parameter
compParamParser :: Parser Expr
compParamParser = compExprParser ||| arithmExprParser ||| numberParser

--- [ Arithmetic Layer ]

-- Parse arithmetic expression
arithmExprParser :: Parser Expr
arithmExprParser = operationParser ||| numberParser ||| subExprParser arithmExprParser

-- Parse arithmetic operation
operationParser :: Parser Expr
operationParser = parseEachOperator arithmOperators parseOneOp 
    where 
        parseOneOp :: String -> Parser Expr
        parseOneOp = duoOperationParser arithmPrevParamParser arithmExprParser

-- Parse arithmetic operation first parameter
arithmPrevParamParser :: Parser Expr
arithmPrevParamParser = subExprParser arithmExprParser ||| numberParser

--- [ Common functions ]

-- Parse any dual operation.
-- The function takes a parser for the first and second parameter and the
--  operator symbol (e.g. "+") and return an Expr structure
duoOperationParser :: Parser Expr -> Parser Expr -> String -> Parser Expr
duoOperationParser prevParam postParam op = do
    x <- prevParam -- first parameter
    _ <- stringTok op -- operator
    y <- postParam -- second parameter
    pure $ Duo (strToOp op) x y

-- Parse any unary operation.
-- The function takes a parser for the parameter and the
--  operator symbol (e.g. "not") and return an Expr structure
unoOperationParser :: Parser Expr -> String -> Parser Expr
unoOperationParser paramParser op = do
    _ <- stringTok op
    a <- paramParser
    pure $ Uno (strToOp op) a

-- Parse the content of an expression under two brackets using the parser (a)
--  and return the result in a SubExpr object
subExprParser :: Parser Expr -> Parser Expr
subExprParser a = SubExpr <$> (bracket a) <* spaces

-- Parse a number and return a lambda Builder in a Var object
numberParser :: Parser Expr
numberParser = do
    nb <- int
    _ <- spaces -- skip spaces
    pure $ Var $ intToLam nb

-- Using a list of operator and a parser given as parameter, 
--    call the parser for each operator in the array until the operation succeed
parseEachOperator :: [Operator] -> (String -> Parser Expr) -> Parser Expr
parseEachOperator operatorList parser = parseEach operatorList $ \op -> parser $ operatorStr op
