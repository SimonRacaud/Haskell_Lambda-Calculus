module Part1 where

import Parser
import Data.Builder
import Data.Lambda

{- 
    Part I
    Lambda expressions Parser 
-}

{-  [ BNF ]

    <lambda> ::= <wrappedFunction> <lambda> | <wrappedFunction> | <function>
    <wrappedFunction> ::= "(" <function> ")"
    <function> ::= "/" <variables> "." <applicationTerm>
    <applicationTerm> ::= <item> | <item> <applicationTerm>
    <item> ::= <terms> | "(" <expression> ")" | <function>
    <expression> ::= <applicationTerm> | <function>
    <variables> ::= <letter> <variables> | <letter>
    <terms> ::= <letter> <terms> | <letter>
    <letter> ::= "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"

-}

lambda :: Parser Builder
lambda =
    let funcList = foldBuilders $ list1 wrappedFunction
    in ((ap <$> funcList) <*> function) ||| funcList

wrappedFunction :: Parser Builder
wrappedFunction = (between (is '(') (is ')') function)

function :: Parser Builder
function =  do
    _ <- is 'λ'
    params <- variables
    _ <- is '.'
    body <- applicationTerm
    return $ params body

applicationTerm :: Parser Builder
applicationTerm = foldBuilders $ list1 item

foldBuilders :: Parser [Builder] -> Parser Builder
foldBuilders lst = fold <$> lst
    where fold (x:xs) = foldl (ap) x xs -- Append builders from left to right
          fold _ = error "Programming error"

expression :: Parser Builder
expression = applicationTerm ||| function

item :: Parser Builder
item = terms ||| (between (is '(') (is ')') expression) ||| function

variables :: Parser (Builder -> Builder)
variables = do
    letters <- list1 letter
    return $ foldVariables letters
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
