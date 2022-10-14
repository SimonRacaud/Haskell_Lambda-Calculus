module Part2Complex where

import Parser hiding (lower)
import Data.Lambda
import Data.Builder

import qualified Part2Arithmetic as Arithm
import qualified Part2Logic as Logic

import Data.Char
import Text.Read
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

complexParser :: Parser Lambda
complexParser = do
    tree <- stmtParser -- Parse string to an expression tree
    build <$> resolveStmt tree -- Reduce the tree to one lambda expression


---- [ Expression evaluator ]

--- [ Logic layer ]

traceExpr :: Expr -> (a -> a)
traceExpr expr = trace ("Fail to resolve expression: " ++ show expr)

-- Resolve statement
resolveStmt :: Stmt -> Parser Builder
resolveStmt (Expr e) = toBuilder $ resolve e
    where 
        toBuilder (Var v) = pure $ v
        toBuilder expr = traceExpr expr $ Parser.fail UnexpectedEof -- Fail to resolve expression
resolveStmt (If c a b) = resolveIfCond c a b

-- resolve 'If then else' condition
resolveIfCond :: Expr -> Stmt -> Stmt -> Parser Builder
resolveIfCond c a b = do
        cond <- (getVar $ resolve c)
        pos <- resolveStmt a
        neg <- resolveStmt b
        pure $ ((Logic.genIf `ap` cond) `ap` pos) `ap` neg
    where
        getVar :: Expr -> Parser Builder 
        getVar (Var v) = pure $ v
        getVar e = traceExpr e $ Parser.fail UnexpectedEof -- Fail to resolve expression
    
--- [ Comparison & Arithmetic Layers ]

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

--- Resolve a specific calculation for the whole tree
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

-- Resolve an operation (Duo)
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

resolveUnop :: Op -> Expr -> Expr
resolveUnop op (Var a) = Var $ solverUnop op a -- Resolve operation
resolveUnop op (SubExpr e) = resolveUnop op $ resolve e
resolveUnop op (Duo o a b) = Duo o (resolveUnop op a) (resolveOperator op b)
resolveUnop op (Uno o a) = resolveUnop op $ resolveUnop o a --go a
    -- where
    --     go (Var x) = solverUnop op $ solverUnop o x -- Double Unop
    --     go (Uno o2 x) = resolveUnop op $ resolveUnop o $ resolveUnop o2 x
    --     go (Duo o2 x y) = Duo o2 (resolveUnop op $ resolveUnop op x) (resolveOperator op y)

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

solverUnop :: Op -> Builder -> Builder
solverUnop NOT a = Logic.genNot `ap` a
solverUnop _ _ = error "Unexpected error"

---- [ Comparison Operations ]

-- | x <= y = LEQ = λmn.isZero (minus m n)
leq :: Builder
leq = lam 'm' $ lam 'n' $ isZero `ap` (Arithm.minus `ap` (term 'm') `ap` (term 'n'))

ueq :: Builder -- (<=)
ueq = Logic.genOr `ap` greater `ap` eq

greater :: Builder -- (>)
greater = Logic.genNot `ap` leq

lower :: Builder -- (<)
lower = Logic.genAnd `ap` leq `ap` (Logic.genNot `ap` eq)

-- | x == y = EQ = λmn.and (LEQ m n) (LEQ n m)
eq :: Builder
eq = lam 'm' $ lam 'n' $ (Logic.genAnd 
                            `ap` (leq `ap` (term 'm') `ap` (term 'n')) 
                            `ap` (leq `ap` (term 'n') `ap` (term 'm')))

neq :: Builder -- (!=)
neq = Logic.genNot `ap` eq

-- | isZero = λn.n(λx.False)True
isZero :: Builder
isZero = lam 'n' $ (term 'n') `ap` (lam 'x' Logic.genFalse) `ap` Logic.genTrue

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
boolParser = (stringTok "True" *> (pure $ Var Logic.genTrue)) 
                ||| (stringTok "False" *> (pure $ Var Logic.genFalse)) 
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
