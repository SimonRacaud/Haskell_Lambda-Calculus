module Part1 where

import Parser
import Data.Builder
import Data.Lambda

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
    <variables> ::= <letter> <variables> | <letter>
    <terms> ::= <letter> <terms> | <letter>
    <letter> ::= "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"

    # Long form only: (The parameters of a function can only contains one variable)
    <variables> ::= <letter> 
-}

lambda :: Bool -> Parser Builder
lambda longOnly = do
    let wrappedFunc = foldBuilders <$> list1 (wrappedFunction longOnly)
    (liftA2 (ap) wrappedFunc (function longOnly)) ||| wrappedFunc ||| (function longOnly)

wrappedFunction :: Bool -> Parser Builder
wrappedFunction longOnly = bracket (function longOnly)

function :: Bool -> Parser Builder
function longOnly =  do
    _ <- is 'λ'
    params <- (variables longOnly)
    _ <- is '.'
    body <- (applicationTerm longOnly)
    return $ params body

applicationTerm :: Bool -> Parser Builder
applicationTerm longOnly = foldBuilders <$> list1 (item longOnly)

foldBuilders :: [Builder] -> Builder
foldBuilders (x:xs) = foldl (ap) x xs -- Append builders from left to right
foldBuilders _ = error "Empty list" -- This should never happen

expression :: Bool -> Parser Builder
expression longOnly = (applicationTerm longOnly) ||| (function longOnly)

item :: Bool -> Parser Builder
item longOnly = terms ||| bracket (expression longOnly) ||| (function longOnly)

variables :: Bool -> Parser (Builder -> Builder)
variables True = do
    l <- letter
    pure $ lam l
variables False = do
    letters <- list1 letter
    pure $ foldVariables letters
    where foldVariables (x:xs) = foldl (\a v -> a . (lam v)) (lam x) xs -- Append variables from left to right
          foldVariables _ = error "Programming error"

terms :: Parser Builder
terms = do
    letters <- list1 letter
    return $ foldTerms letters
    where foldTerms (x:xs) = foldl (\a v -> a `ap` (term v)) (term x) xs -- Append terms from left to right
          foldTerms _ = error "Programming error"

letter :: Parser Char
letter = oneof "abcdefghijklmnopqrstuvwxyz"
