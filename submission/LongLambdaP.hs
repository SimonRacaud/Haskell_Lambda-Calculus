module LongLambdaP where

import Parser
import Data.Builder

{- 
    Part I - Exercice 1
    Parser for verbose lambda expressions
-}

-- Construct parsers for lambda calculus expression components 
--      (“λ”, “.”, “(“, “)”, “x”, “y”, “z”, etc.)

-- parse (build <$> pLambda) "(λx.x)"

pLambda :: Parser Builder
pLambda = (between (is '(') (is ')') lambda) ||| lambda

lambda :: Parser Builder
lambda = do
    _ <- is 'λ'
    params <- getParams
    _ <- is '.'
    body <- getBody
    return $ params body

getParams :: Parser (Builder -> Builder)
getParams = do
    letters <- munch1 isLetter
    return $ parseParams letters
    where 
        parseParams (x:xs) = foldr (\v a -> a . (lam v)) (lam x) xs

getLetter :: Parser Char
getLetter = oneof "abcdefghijklmnopqrstuvwxyz"

isLetter :: Char -> Bool
isLetter c = c >= 'a' && c <= 'z'

getBody :: Parser Builder
getBody = parseBody <$> list1 getBodyEl -- TODO : to impove
    where parseBody (x:xs) = foldr (\v a -> ap v a) x xs

getBodyEl :: Parser Builder
getBodyEl = pLambda ||| (term <$> getLetter)