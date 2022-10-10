module LongLambdaP where

import Parser
import Data.Builder
import Data.Lambda

import Debug.Trace

{- 
    Part I
    Lambda expressions Parser 
-}

functionList :: Parser Builder
functionList =
    let funcList = foldBuilders $ list1 functionB
    in ((ap <$> funcList) <*> function) ||| funcList

functionB :: Parser Builder
functionB = (between (is '(') (is ')') function)

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
foldBuilders list = fold <$> list
    where fold (x:xs) = foldr (flip ap) x xs

expression :: Parser Builder
expression = applicationTerm ||| function

item :: Parser Builder
item = terms ||| (between (is '(') (is ')') expression)

variables :: Parser (Builder -> Builder)
variables = do
    letters <- list1 letter
    return $ foldVariables letters
    where foldVariables (x:xs) = foldr (\v a -> a . (lam v)) (lam x) xs

terms :: Parser Builder
terms = do
    letters <- list1 letter
    return $ foldTerms letters
    where foldTerms (x:xs) = foldr (\v a -> a `ap` (term v)) (term x) xs

letter :: Parser Char
letter = oneof "abcdefghijklmnopqrstuvwxyz"
