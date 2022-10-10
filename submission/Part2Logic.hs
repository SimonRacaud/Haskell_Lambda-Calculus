module Part2Logic where

import Parser
import Data.Builder

{-
    Part II - parse arithmetic and logical expressions 
-}


{- Exercice I -}

{- [BNF]

    <stmt> ::= <ifCond> | <expr>

    <ifCond> ::= "if" <expr> "then" <stmt> "else" <stmt>

    <expr> ::= <param> <duop> <expr> | <unop> <expr> | '(' <expr> ')' | <bool>
    <param> ::=  '(' <expr> ')' | <bool>
    <bool> ::= "True" | "False"
    <unop> ::= "not"
    <duop> ::= "and" | "or"
-}

-- TODO: Temporary:
-- Value: True, False, 
-- Suffix cond: and, or, 
-- Prefix cond: not, 
-- Cond: if (...) then (...) else (...)
-- Parses expressions with the correct order of operations ( “()” -> “not” -> “and” -> “or”)

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

-- logicParser :: Parser Builder
-- logicParser = do
--     tree <- stmtParser
--     pure $ genIf

-- parseStmt :: Stmt -> Builder
-- parseStmt (Expr e) = parseExpr e
-- parseStmt (If c a b) = undefined -- TODO ...

parseExpr :: LogicExpr -> Builder
parseExpr expr = convert $ parseOr $ parseAnd $ parseNot $ parseBracket expr
    where convert (Var a) = a
          convert _ = undefined -- should not happen

parseBracket :: LogicExpr -> LogicExpr
parseBracket (SubExpr e) = Var $ parseExpr e
parseBracket (Uno op a) = Uno op (parseBracket a)
parseBracket (Duo op a b) = Duo op (parseBracket a) (parseBracket b)
parseBracket a = id a -- do nothing

parseNot :: LogicExpr -> LogicExpr
parseNot (Uno _ (Var b)) = Var $ genNot `ap` b
parseNot (Uno _ (Uno _ b)) = Var $ app (parseNot b)
                            where app (Var v) = genNot `ap` (genNot `ap` v)
                                  app (Uno _ v) = genNot `ap` (genNot `ap` (app (parseNot v)))
                                  app _ = undefined -- unexpected error
parseNot (Uno _ (Duo op (Var a) b)) = Duo op (Var $ genNot `ap` a) (parseNot b)
parseNot (Duo op a b) = Duo op (parseNot a) (parseNot b)
parseNot a = id a -- do nothing

parseAnd :: LogicExpr -> LogicExpr
parseAnd (Duo And (Var a) (Var b)) = Var $ genAnd `ap` a `ap` b
parseAnd (Duo And (Var a) (Duo op (Var b) c)) = Duo op (Var $ a `ap` b) (parseAnd c)
-- parseAnd (Duo And a b) = Var $ genAnd `ap` (parseAnd a) `ap` (parseAnd b)
    -- where parseSubDuo (Duo Or a b) = parseSubDuo a
parseAnd (Duo Or a b) = Duo Or (parseAnd a) (parseAnd b)
parseAnd a = id a -- do nothing

-- TODO: Memo doing => parseStmt + test for error

parseOr :: LogicExpr -> LogicExpr
parseOr (Duo Or (Var a) (Var b)) = Var $ genOr `ap` a `ap` b
parseOr x = id x -- Do nothing

--- Parse the input to a tree of statements and expressions

--- >>> parse stmtParser "not True or False and False"
--- Result >< Expr (Uno Not (Duo Or (Var True) (Duo And (Var False) (Var False))))
---
--- >>> parse stmtParser "not    not True"
--- Result >< Expr (Uno Not (Uno Not (Var True)))
---
stmtParser :: Parser Stmt
stmtParser = ifCondParser ||| (Expr <$> exprParser)

ifCondParser :: Parser Stmt
ifCondParser = do
    _ <- spaces -- skip spaces
    _ <- string "if"
    cond <- exprParser
    _ <- string "then"
    positif <- stmtParser
    _ <- string "else"
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

-- data LogicTree a = Empty
--             | List [LogicTree a] -- Used before evaluating condition priorities
--             | Bool a -- True or False
--             | Not a (LogicTree a)
--             | And a (LogicTree a) (LogicTree a) 
--             | Or a (LogicTree a) (LogicTree a)
--             | If a (LogicTree a) (LogicTree a) (LogicTree a)

-- parseTerms :: Parser (LogicTree Builder)
-- parseTerms = list1 parseTerm >>= (\list -> pure $ List list)

-- parseTerm :: Parser (LogicTree Builder)
-- parseTerm = parseTrue 
--                 ||| parseFalse 
--                 ||| parseAnd 
--                 ||| parseOr 
--                 ||| parseParentheses
--                 ||| parseNot
--                 ||| parseIf

-- parseParentheses :: Parser (LogicTree Builder)
-- parseParentheses = do
--     expr <- between (is '(') (is ')') parseTerms
--     pure $ expr

-- parseTrue :: Parser (LogicTree Builder)
-- parseTrue = do 
--     _ <- string "True"
--     pure $ Bool $ genTrue

-- parseFalse :: Parser (LogicTree Builder)
-- parseFalse = do
--     _ <- string "False"
--     pure $ Bool $ genFalse

-- -- λxy. IF x y FALSE
-- parseAnd :: Parser (LogicTree Builder)
-- parseAnd = do
--     _ <- string "and"
--     pure $ And (genAnd) Empty Empty

-- parseOr :: Parser (LogicTree Builder)
-- parseOr = do
--     _ <- string "or"
--     pure $ Or (genOr) Empty Empty

-- parseNot :: Parser (LogicTree Builder)
-- parseNot = do
--     _ <- string "not"
--     pure $ Not (genNot) Empty

-- parseIf :: Parser (LogicTree Builder)
-- parseIf = do
--     _ <- string "if"
--     cond <- list1 parseTerm
--     _ <- string "then"
--     positif <- list1 parseTerm
--     _ <- string "else"
--     negatif <- list1 parseTerm
--     pure $ If (genIf) (List cond) (List positif) (List negatif)
