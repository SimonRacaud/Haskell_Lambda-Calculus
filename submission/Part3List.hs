module Part3List where

import Part1 (lambda)
import Part2Complex (complexParser, genTrue, genFalse)

import Parser
import Data.Builder
import Data.Lambda

-- Simple parser of list. The list can only contains numbers or boolean
listParser :: Parser Builder
listParser = nullParser ||| (do
    l <- array $ itemParser
    case l of
        [] -> pure $ genList
        _ -> pure $ foldr (\v a -> genCons `ap` v `ap` a) genList l
        
    )

itemParser :: Parser Builder
itemParser = listParser 
    ||| (lamToBuilder <$> complexParser)
    ||| (lambda False)
    -- ComplexParser will take care of boolean and numbers

-- Parse a sequence of function (isNull, head, tail, cons)
functionListParser :: Parser Lambda
functionListParser = do
    funcList <- (list1 parseFunction)
    l <- listParser
    pure $ build $ foldr (ap) l funcList
    where 
        parseFunction = spaces *>
                    isNullParser
                    ||| headParser
                    ||| tailParser
                    ||| restParser
                    ||| consParser

nullParser :: Parser Builder
nullParser = (spaces *> string "null" <* spaces) >> (pure genList)

isNullParser :: Parser Builder
isNullParser = string "isNull" >> (pure genIsNull)

headParser :: Parser Builder
headParser = string "head" >> (pure genHead)

tailParser :: Parser Builder
tailParser = string "tail" >> (pure genTail)

restParser :: Parser Builder
restParser = string "rest" >> (pure genTail)

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
