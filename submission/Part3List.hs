module Part3List where

import Part1 (lambda)
import Part2Complex (complexParser, genTrue, genFalse)

import Parser
import Data.Builder
import Data.Lambda

-- Simple parser of a list of expression between square brackets [].
listParser :: Parser Builder
listParser = nullParser ||| (do
    l <- array $ itemParser
    case l of
        [] -> pure $ genList
        _ -> pure $ foldr (\v a -> genCons `ap` v `ap` a) genList l
    )

-- Parse a list item (a list, complex expression or lambda expression)
itemParser :: Parser Builder
itemParser = listParser 
    ||| (lamToBuilder <$> complexParser)
    ||| (lambda False)
    -- ComplexParser will take care of boolean and numbers

-- Parse a sequence of function (isNull, head, tail, cons) and list
functionListParser :: Parser Lambda
functionListParser = do
    -- Extract list of functions
    funcList <- (list1 parseFunction)
    -- Expract list of expressions
    l <- listParser
    -- Generate final Lambda
    pure $ build $ foldr (ap) l funcList
    where 
        parseFunction = spaces *>
                    isNullParser
                    ||| headParser
                    ||| tailParser
                    ||| restParser
                    ||| consParser

-- Parse null function
nullParser :: Parser Builder
nullParser = (spaces *> string "null" <* spaces) >> (pure genList)

-- Parse isNull function
isNullParser :: Parser Builder
isNullParser = string "isNull" >> (pure genIsNull)

-- Parse head function
headParser :: Parser Builder
headParser = string "head" >> (pure genHead)

-- Parse tail function
tailParser :: Parser Builder
tailParser = string "tail" >> (pure genTail)

-- Alias to tail
restParser :: Parser Builder
restParser = string "rest" >> (pure genTail)

-- Parse cons function and his parameter
consParser :: Parser Builder
consParser = do
    _ <- stringTok "cons"
    item <- itemParser
    pure $ genCons `ap` item

--- [ Lambda calculus functions ]

-- [] = null = λcn.n
genList :: Builder
genList = lam 'c' $ lam 'n' (term 'n')

-- cons = λhtcn.ch(tcn)
genCons :: Builder
genCons = lam 'h' $ lam 't' $ lam 'c' $ lam 'n' $ (term 'c') `ap` (term 'h') 
                `ap` ((term 't') `ap` (term 'c') `ap` (term 'n'))

-- isNull = λl.l(λht.False) True
genIsNull :: Builder
genIsNull = lam 'l' $ (term 'l') `ap` (lam 'h' $ lam 't' genFalse) `ap` genTrue

-- head = λl.l(λht.h) False
genHead :: Builder
genHead = lam 'l' $ (term 'l') `ap` (lam 'h' $ lam 't' (term 'h')) `ap` genFalse

-- tail = λlcn.l(λhtg.gh(tc))(λt.n)(λht.t)
genTail :: Builder
genTail = lam 'l' $ lam 'c' $ lam 'n' $ (term 'l') `ap` 
            (lam 'h' $ lam 't' $ lam 'g' $ (term 'g') `ap` (term 'h') 
                `ap` ((term 't') `ap` (term 'c'))) `ap`
            (lam 't' (term 'n')) `ap`
            (lam 'h' $ lam 't' (term 't'))
