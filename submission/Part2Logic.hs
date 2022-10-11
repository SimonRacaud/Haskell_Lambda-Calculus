module Part2Logic where

import Parser
import Data.Builder

import Debug.Trace

{-
    Part II - parse arithmetic and logical expressions 
-}


{- Exercice I -}

{- [BNF]

    <stmt> ::= <ifCond> |  '(' <ifCond> ')' | <expr>

    <ifCond> ::= "if" <expr> "then" <stmt> "else" <stmt>

    <expr> ::= <param> <duop> <expr> | <unop> <expr> | '(' <expr> ')' | <bool>
    <param> ::=  '(' <expr> ')' | <bool>

    <bool> ::= "True" | "False"
    <unop> ::= "not"
    <duop> ::= "and" | "or"
-}

---- [ Expression builders ]

-- λxy.x
genTrue :: Builder
genTrue = boolToLam True

-- λxy.y 
genFalse :: Builder
genFalse = boolToLam False

-- If: λbtf.b t f
genIf :: Builder
genIf = lam 'b' $ lam 't' $ lam 'f' ((term 'b') `ap` (term 't') `ap` (term 'f'))

-- λxy. IF x  y FALSE
genAnd :: Builder
genAnd = lam 'x' $ lam 'y' (genIf `ap` (term 'x') `ap` (term 'y') `ap` genFalse)

-- λxy. IF x TRUE y 
genOr :: Builder
genOr = lam 'x' $ lam 'y' (genIf `ap` (term 'x') `ap` genTrue `ap` (term 'y'))

-- λx. IF x FALSE TRUE
genNot :: Builder
genNot = lam 'x' (genIf `ap` (term 'x') `ap` genFalse `ap` genTrue)

---- [ Data ]

data LogicExpr = Var Builder
                | Uno Unop LogicExpr
                | Duo Duop LogicExpr LogicExpr
                | SubExpr LogicExpr -- Brackets ()
                deriving Show

data Unop = Not deriving Show
data Duop = And | Or deriving Show
data Stmt = Expr LogicExpr 
            | If LogicExpr Stmt Stmt          
            deriving Show

---- [ Parser ]

logicParser :: Parser Builder
logicParser = do
    tree <- stmtParser
    case parseStmt tree of
        (Just a) -> pure $ a
        Nothing -> Parser.fail UnexpectedEof -- Expression resolution failure

parseStmt :: Stmt -> Maybe Builder
parseStmt (Expr e) = parseExpr e
parseStmt (If c a b) = parseIfCond c a b

parseIfCond :: LogicExpr -> Stmt -> Stmt -> Maybe Builder
parseIfCond c a b = do
    parseExpr c >>= (\cond ->
        parseStmt a >>= (\pos ->
            parseStmt b >>= (\neg ->
                Just $ ((genIf `ap` cond) `ap` pos) `ap` neg
                )))

parseExpr :: LogicExpr -> Maybe Builder
parseExpr expr = convert $ parseOr <$> parseAnd <$> parseNot <$> parseBracket expr
    where 
        convert :: Maybe LogicExpr -> Maybe Builder
        convert (Just (Var a)) = Just a
        convert a = trace ("Fail to reduce expression: "++show a) Nothing

parseBracket :: LogicExpr -> Maybe LogicExpr
parseBracket (SubExpr e) = Var <$> parseExpr e -- Parse expression
parseBracket (Uno op a) = (Uno op) <$> (parseBracket a) -- Propagate
parseBracket (Duo op a b) = (Duo op) <$> (parseBracket a) <*> (parseBracket b) -- Propagate
parseBracket a = Just a -- do nothing

parseNot :: LogicExpr -> LogicExpr
parseNot (Uno _ (Var b)) = Var $ genNot `ap` b -- Parse data
parseNot (Uno _ (Uno _ b)) = applyNot $ parseNot b
    where 
        applyNot :: LogicExpr -> LogicExpr
        applyNot (Var a) = Var $ genNot `ap` (genNot `ap` a)
        applyNot (Duo op (Var a) c) = Duo op (Var $ genNot `ap` (genNot `ap` a)) c
        applyNot e = trace ("parseNot: unexpected error. " ++ (show e)) (error "internal error")
parseNot (Uno _ (Duo op (Var a) b)) = Duo op (Var $ genNot `ap` a) (parseNot b) -- Parse data & Propagate
parseNot (Duo op a b) = Duo op (parseNot a) (parseNot b) -- Propagate
parseNot a = id a -- do nothing

parseAnd :: LogicExpr -> LogicExpr
parseAnd (Duo And (Var a) (Var b)) = Var $ genAnd `ap` a `ap` b -- Parse data
parseAnd (Duo And (Var a) (Duo op (Var b) c)) = Duo op (Var $ genAnd `ap` a `ap` b) (parseAnd c) -- Parse data & Propagate
parseAnd (Duo Or a b) = Duo Or (parseAnd a) (parseAnd b) -- Propagate
parseAnd a = id a -- do nothing

parseOr :: LogicExpr -> LogicExpr
parseOr (Duo Or (Var a) (Var b)) = Var $ genOr `ap` a `ap` b -- Parse data
parseOr (Duo Or (Var a) b) = Var $ genOr `ap` a `ap` (applyOr $ parseOr b) -- Resolve sub-expressions
    where
        applyOr :: LogicExpr -> Builder
        applyOr (Var v) = v
        applyOr e = trace ("parseOr: unexpected error. " ++ (show e)) (error "internal error")
parseOr x = id x -- Do nothing

--- Parse the input to a tree of statements and expressions

--- >>> parse stmtParser "not True or False and False"
--- Result >< Expr (Uno Not (Duo Or (Var True) (Duo And (Var False) (Var False))))
---
--- >>> parse stmtParser "not    not True"
--- Result >< Expr (Uno Not (Uno Not (Var True)))
---
stmtParser :: Parser Stmt
stmtParser = spaces *>
    (ifCondParser ||| (between (is '(') (is ')') ifCondParser) ||| (Expr <$> exprParser))

ifCondParser :: Parser Stmt
ifCondParser = do
    _ <- string "if"
    cond <- exprParser
    _ <- spaces -- skip spaces
    _ <- string "then"
    _ <- spaces -- skip spaces
    positif <- stmtParser
    _ <- spaces -- skip spaces
    _ <- string "else"
    _ <- spaces -- skip spaces
    negatif <- stmtParser
    pure $ If cond positif negatif

exprParser :: Parser LogicExpr
exprParser = spaces *> (duopParser
            ||| unopParser
            ||| subExprParser
            ||| boolParser)

 -- Restricted version of exprParser to avoid infinite recursion (duop)
paramParser :: Parser LogicExpr
paramParser = subExprParser
            ||| boolParser

subExprParser :: Parser LogicExpr
subExprParser = SubExpr <$> (between (is '(') (is ')') exprParser)

unopParser :: Parser LogicExpr
unopParser = do
    _ <- string "not"
    a <- exprParser
    pure $ Uno Not a

duopParser :: Parser LogicExpr
duopParser = andParser ||| orParser

andParser :: Parser LogicExpr
andParser = do
    a <- paramParser -- Restricted version of exprParser to avoid infinite recursion
    _ <- spaces -- skip spaces
    _ <- string "and"
    _ <- spaces -- skip spaces
    b <- exprParser
    pure $ Duo And a b

orParser :: Parser LogicExpr
orParser = do
    a <- paramParser -- Restricted version of exprParser to avoid infinite recursion
    _ <- spaces -- skip spaces
    _ <- string "or"
    _ <- spaces -- skip spaces
    b <- exprParser
    pure $ Duo Or a b

boolParser :: Parser LogicExpr
boolParser = (string "True" *> (pure $ Var genTrue)) 
            ||| (string "False" *> (pure $ Var genFalse))
