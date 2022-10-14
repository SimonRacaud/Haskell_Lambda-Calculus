-- | The simple Parser types.
module Parser where

import Control.Applicative hiding (some,many)
import Control.Monad hiding (fail)
import Data.Char
import Prelude hiding (fail)

-- | The input to a Parser.
type Input = String

-- | The result of a parse
data ParseResult a = Result Input a -- | The result of a successful parse.
  | Error ParseError -- | The result of a failed parse.

instance Functor ParseResult where
  fmap f (Result i a) = Result i $ f a
  fmap _ (Error e)    = Error e

instance Show a => Show (ParseResult a) where
  show (Result i a) = "Result >" ++ i ++ "< " ++ show a
  show (Error e)    = show e

instance Eq a => Eq (ParseResult a) where
  (Result il al) == (Result ir ar) = il == ir && al == ar
  (Error el)     == (Error er)     = el == er
  _              == _              = False

data ParseError = UnexpectedChar Char
  | ExpectedEof Input
  | UnexpectedEof
  deriving (Eq,Show)

-- | The Parser type.
newtype Parser a = P {
  parse :: Input -> ParseResult a -- | Runs the Parser on the input.
  }

instance Functor Parser where
  fmap f (P p) = P $ fmap f . p

instance Applicative Parser where
  pure = P . flip Result
  ff <*> fa = ff >>= (<$> fa)

instance Monad Parser where
  fa >>= f = P (\i ->
    case parse fa i of
      Result i' a -> parse (f a) i'
      Error e     -> Error e)

fail :: ParseError -> Parser a
fail = P . const . Error

-- Get the first character of the input string
-- >>> parse (get) "abc"
-- Result >bc< 'a'
get :: Parser Char
get = P go
  where
    go (c:i) = Result i c
    go []    = Error UnexpectedEof

-- Check whenever the first check of a string satisfy a condition
-- >>> parse (satisfy (\c -> c == 'b')) "abc"
-- UnexpectedChar 'a'
--
-- >>> parse (satisfy (\c -> c == 'a')) "abc"
-- Result >bc< 'a'
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do
  c <- get
  if f c then pure c else fail (UnexpectedChar c)

-- Parse the first argument. If error, parse the second
-- >>> parse ((is 'a') ||| (is 'b')) "abc"
-- Result >bc< 'a'
(|||) :: Parser a -> Parser a -> Parser a
lfa ||| rfa = P (\i ->
  case parse lfa i of
    Error _ -> parse rfa i
    r       -> r)

-- 
-- >>> parse (list (is 'a')) "aabca"
-- Result >bca< "aa"
--
-- >>> parse (list (is 'a')) "bca"
-- Result >bca< ""
list :: Parser a -> Parser [a]
list fa = list1 fa ||| pure []

-- >>> parse (list1 (is 'a')) "bca"
-- UnexpectedChar 'b'
--
-- >>> parse (list1 (is 'a')) "abca"
-- Result >bca< "a"
list1 :: Parser a -> Parser [a]
list1 fa = liftA2 (:) fa (list fa)

-- >>> parse (munch1 (\c -> c == 'a')) "aabca"
-- Result >bca< "aa"
--
-- >>> parse (munch1 (\c -> c == 'b')) "aabca"
-- UnexpectedChar 'a'
munch1 :: (Char -> Bool) -> Parser String
munch1 = list1 . satisfy

-- check if the first character of a string is equal to the character given as parameter
-- >>> parse (is 'a') "bca"
-- UnexpectedChar 'b'
--
-- >>> parse (is 'b') "bbca"
-- Result >bca< 'b'
is :: Char -> Parser Char
is = satisfy . (==)

-- >>> parse (string "bb") "bbca"
-- Result >ca< "bb"
--
-- >>> parse (string "ca") "bbca"
-- UnexpectedChar 'b'
--
-- Try to match a string
--
string :: String -> Parser String
string = traverse is

-- Check if first character is a space
-- >>> parse (space) "bbca"
-- UnexpectedChar 'b'
--
-- >>> parse (space) "  bbca"
-- Result > bbca< ' ' 
space :: Parser Char
space = satisfy isSpace

-- >>> parse (spaces) "  bbca"
-- Result >bbca< "  "
spaces :: Parser String
spaces = list space

-- Extract expression between to others
-- >>> parse (between (is '(') (is ')') (get)) "(x)"
-- Result >< 'x'
between :: Parser open -> Parser close -> Parser a -> Parser a
between fo fc fa = fo *> fa <* fc

-- Parse expression under brackets
bracket :: Parser a -> Parser a
bracket = between (is '(') (is ')') 

-- >>> (parserToReadS (get)) "123"
-- [('1',"23")]
parserToReadS :: Parser a -> ReadS a
parserToReadS = (go .) . parse
  where
    go (Result i a) = [(a,i)]
    go (Error _)    = []

-- >>> parse (oneof "81") "123"
-- Result >23< '1'
oneof :: String -> Parser Char
oneof = satisfy . flip elem

-- >>> parse (noneof "81") "123"
-- UnexpectedChar '1'
--
-- >>> parse (noneof "87") "123"
-- Result >23< '1'
noneof :: String -> Parser Char
noneof = satisfy . flip notElem

-- Chaining parsing attempts
parseEach :: [a] -> (a -> Parser b) -> Parser b
parseEach [x] f = f x
parseEach (x:xs) f = f x ||| parseEach xs f
parseEach [] _ = fail UnexpectedEof -- Empty list

-- | `chain p op` parses 1 or more instances of `p` separated by `op`
-- | (see chainl1 from Text.Parsec)
-- | This can be a very useful parser combinator
chain :: Parser a -> Parser (a -> a -> a) -> Parser a
chain p op = p >>= rest
 where
  rest a =
    (do
        f <- op
        b <- p
        rest (f a b)
      )
      ||| pure a

-- | Produces a parser that always fails with 'UnexpectedChar' using the given
-- character.
unexpectedCharParser :: Char -> Parser a
unexpectedCharParser = fail . UnexpectedChar

--- Check if the result of parse is an error
isErrorResult :: ParseResult a -> Bool
isErrorResult (Error _) = True
isErrorResult _         = False

readInt :: String -> Maybe (Int, String)
readInt s = case reads s of
  [(x, rest)] -> Just (x, rest)
  _           -> Nothing

-- | Parse numbers as int until non-digit
--
-- >>> parse int "abc"
-- Result >bc< 'a'
--
-- >>> isErrorResult (parse int "")
-- True
--
-- >>> isErrorResult (parse int "a")
-- True
int :: Parser Int
int = P f
 where
  -- This is okay because the case statement is small
  f "" = Error UnexpectedEof
  f x  = case readInt x of
    Just (v, rest) -> Result rest v
    Nothing        -> Error $ UnexpectedChar (head x)

-- | Write a parser that asserts that there is no remaining input.
--
-- >>> parse eof ""
-- Result >< ()
--
-- >>> isErrorResult (parse eof "abc")
-- True
eof :: Parser ()
eof = P f
 where
  f "" = Result "" ()
  f x  = Error $ ExpectedEof x

-- | Return a parser that produces a lower-case character but fails if:
--   * the input is empty;
--   * the produced character is not lower-case.
--
lower :: Parser Char
lower = oneof ['a' .. 'z']

-- | Return a parser that produces an upper-case character but fails if:
--   * the input is empty; or
--   * the produced character is not upper-case.
--
upper :: Parser Char
upper = oneof ['A' .. 'Z']

-- | Return a parser that produces an alpha character but fails if:
--   * the input is empty; or
--   * the produced character is not alpha.
--
alpha :: Parser Char
alpha = lower ||| upper

-- | Applies the given parser, then parses 0 or more
-- spaces, then produces the result of the original parser.
--
-- >>> parse (tok (is 'a')) "a bc"
-- Result >bc< 'a'
--
-- >>> parse (tok (is 'a')) "abc"
-- Result >bc< 'a'
tok :: Parser a -> Parser a
tok p = p <* spaces 

-- | Parses the given string, followed by 0 or more
-- spaces.
--
-- >>> parse (stringTok "abc") "abc  "
-- Result >< "abc"
--
-- >>> isErrorResult (parse (stringTok "abc") "bc  ")
-- True
stringTok :: String -> Parser String
stringTok str = (string str) <* spaces

-- | Parses the given char followed by 0 or more spaces.
-- >>> parse (charTok 'a') "abc"
-- Result >bc< 'a'
--
-- >>> isErrorResult (parse (charTok 'a') "dabc")
-- True
charTok :: Char -> Parser Char
charTok c = is c <* spaces

