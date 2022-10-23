module Part1 where

import Parser
import Data.Builder

import Control.Applicative

{- 
    Part I
    Lambda expressions Parser 
-}

{-  [ BNF ]

    # Long and Short form parser
    <lambda> ::= <wrappedFunction> <lambda> | <wrappedFunction> | <function>
    <wrappedFunction> ::= "(" <function> ")"
    <function> ::= "λ" <variables> "." <applicationTerm>
    <applicationTerm> ::= <item> | <item> <applicationTerm>
    <item> ::= <terms> | "(" <expression> ")" | <function>
    <expression> ::= <applicationTerm> | <function>
    <terms> ::= <letter> <terms> | <letter>
    <letter> ::= 'a' ... 'z'

    # Short form
    <variables> ::= <letter> <variables> | <letter>

    # Long form only: (The parameters of a function can only contains one variable)
    <variables> ::= <letter> 
-}

-- Lambda expression parser
-- The boolean parameter define if the parser only works on long form
lambda :: Bool -> Parser Builder
lambda longOnly = do
    -- Try to parse every lambda expression under brackets (e.g. "(Exp1)(Exp2)(Exp3)..." )
    let wrappedFunc = foldBuilders <$> list1 (wrappedFunction longOnly)
    -- Parse a combinaison of wrapped expressions with a non-wrapped at the end: e.g. "(E1)(E2)(E3)E4"
    --  or parse a combinaison of wrapped expressions: e.g. "(E1)(E2)(E3).."
    --  or parse a single non-wrapped expression: e.g. "\x.x"
    (liftA2 (ap) wrappedFunc (function longOnly)) ||| wrappedFunc ||| (function longOnly)

-- Parse expression wrapped under brackets
wrappedFunction :: Bool -> Parser Builder
wrappedFunction longOnly = bracket (function longOnly)

-- Parse lambda function
function :: Bool -> Parser Builder
function longOnly =  do
    _ <- is 'λ'
    params <- (variables longOnly) -- Parameter(s)
    _ <- is '.'
    body <- (applicationTerm longOnly) -- Body
    return $ params body -- Combine

-- Parse function body
applicationTerm :: Bool -> Parser Builder
applicationTerm longOnly = foldBuilders <$> list1 (item longOnly)

-- Combine array of builder to one
foldBuilders :: [Builder] -> Builder
foldBuilders (x:xs) = foldl (ap) x xs -- Append builders from left to right
foldBuilders _ = error "Empty list" -- This should never happen

-- Parse function body expression
expression :: Bool -> Parser Builder
expression longOnly = (applicationTerm longOnly) ||| (function longOnly)

-- Parse function body item
-- Letters, wrapper expression or single function
item :: Bool -> Parser Builder
item longOnly = terms ||| bracket (expression longOnly) ||| (function longOnly)

-- Parse function variables
variables :: Bool -> Parser (Builder -> Builder)
    -- Long form only:
variables True = do
    l <- letter
    pure $ lam l
    -- Short form: (allows multiple variables)
variables False = do
    letters <- list1 letter
    pure $ foldVariables letters
    where foldVariables (x:xs) = foldl (\a v -> a . (lam v)) (lam x) xs -- Append variables from left to right
          foldVariables _ = error "Programming error"

-- Parse function terms
terms :: Parser Builder
terms = do
    letters <- list1 letter
    return $ foldTerms letters
    where foldTerms (x:xs) = foldl (\a v -> a `ap` (term v)) (term x) xs -- Append terms from left to right
          foldTerms _ = error "Programming error"

-- Parse one letter
letter :: Parser Char
letter = oneof "abcdefghijklmnopqrstuvwxyz"
