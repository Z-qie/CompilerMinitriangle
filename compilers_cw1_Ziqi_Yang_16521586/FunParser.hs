{-
Auxiliary Parsers extended by Ziqi Yang, 2022

Functional parsing library by Venanzio Capretta,
based on chapter 13 of "Programming in Haskell" (2nd edition)
Graham Hutton, Cambridge University Press, 2016.

Whats New:
  - Reserved keyword and format Checker for Identifier at parsing time
  - starting variable name letter can be both lower and upper case 
  - items read a word token for error message to indicate the error position
-}

module FunParser where

import Control.Applicative
import Data.Char
import GHC.Base (RuntimeRep(TupleRep))

-- Variable Name Checker -----------------------------------------------------------------------
-- Variable Name cannot be any reserved keyoword
isKeyword :: String -> Bool
isKeyword "let" = True
isKeyword "in" = True
isKeyword "var" = True
isKeyword "if" = True
isKeyword "then" = True
isKeyword "else" = True
isKeyword "while" = True
isKeyword "do" = True
isKeyword "getint" = True
isKeyword "printint" = True
isKeyword "begin" = True
isKeyword "end" = True
isKeyword _ = False

-- keyword checking 
keywordSat :: (String -> Bool) -> Parser String
keywordSat = satisfy identifier

checkKwd :: Parser String
checkKwd = keywordSat (not . isKeyword)

checkKeyword :: Parser String
checkKeyword = token checkKwd

--can be used for both lower and upper case letter starting variable name
ident :: Parser String
ident = do x <- letter
           (do xs <- many alphanum
               return (x:xs))
      <|>
        error "\nParsing Error: Variables must start with a lower or upper case letter!"

identifier :: Parser String
identifier = token ident

-- Basic definitions ----------------------------------------------------------------------
newtype Parser a = P (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) = p

item :: Parser Char
item = P (\inp -> case inp of
                     []     -> []
                     (x:xs) -> [(x,xs)])

-- items read a word token for error message to indicate the error position
items :: Parser String 
items = do many (sat (not.isSpace))

-- Sequencing parsers

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap g pa = P (\src -> [ (g x, src1) | (x,src1) <- parse pa src ])

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure x = P (\src -> [(x,src)])

  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> pa = P (\src -> [ (f x,src2) | (f,src1) <- parse pf src,
                                        (x,src2) <- parse pa src1 ] )

instance Monad Parser where
  -- return :: a -> Parser a
  -- return = pure

  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  pa >>= fpb = P (\src -> [r | (x,src1) <- parse pa src,
                               r <- parse (fpb x) src1 ] )

--Making choices

instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\rsc -> [])

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p1 <|> p2 = P (\src -> case parse p1 src of
                    [] -> parse p2 src
                    rs -> rs)

-- Chosing among many alternatives
choice :: Alternative f => [f a] -> f a
choice = foldl (<|>) empty

{-
Parallel parsing: getting the results of both parsers
  for ambiguous grammars
Use with caution: it can cause inefficiency
-}

infixl 3 <||>
(<||>) :: Parser a -> Parser a -> Parser a
p1 <||> p2 = P (\inp -> parse p1 inp ++ parse p2 inp)

-- Derived primitives

-- verify that the parsed object satisfy a condition
satisfy :: Parser a -> (a -> Bool) -> Parser a
satisfy p cond = do x <- p
                    if cond x then return x else empty

sat :: (Char -> Bool) -> Parser Char
sat = satisfy item

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)


nat :: (Num int, Read int) => Parser int
nat = do xs <- some digit
         return (read xs)

int :: (Num int, Read int) => Parser int
int = do char '-'
         n <- nat
         return (-n)
      <|> nat

-- Handling spacing

space :: Parser ()
space = do many (sat isSpace)
           return ()



token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

natural :: (Num int, Read int) => Parser int
natural = token nat

integer :: (Num int, Read int) => Parser int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

parens :: Parser a -> Parser a
parens p = do symbol "("
              x <- p
              symbol ")"
              return x

-- Example: parsing a list of integers
nats :: Parser [Integer]
nats = do symbol "["
          n <- natural
          ns <- many (do symbol ","
                         natural)
          symbol "]"
          return (n:ns)


