{---------------------------------------------------------------------

           A HASKELL LIBRARY OF MONADIC PARSER COMBINATORS

                          17th April 1997

               Graham Hutton              Erik Meijer
          University of Nottingham   University of Utrecht

This Haskell 1.3 library is derived from our forthcoming JFP article
"Monadic Parsing in Haskell".  The library also includes a few extra
combinators that were not discussed in the article for reasons of space:

   o force (used to make "many" deliver results lazily);

   o digit, lower, upper, letter, alphanum (useful parsers);

   o ident, nat, int (useful token parsers).

---------------------------------------------------------------------}

{- -------------------------------------------------------------------

   Some minor modifications were made to this file by ZF and HC for
   use with the CS 556 (Declarative Programming) Proj 03, Fall 2020.
   Primarily, see:
   * the addition of the todelim and todelim_include Parsers
   * a modification of concatParsers to include token

---------------------------------------------------------------------}

module Parselib
   (Parser, item, sat, (+++), string, many, many1, sepby, sepby1,
    chainl, chainl1, char, digit, lower, upper, letter, alphanum,
    symb, ident, nat, int, token, apply, parse, space, integer, natural, mplus,
    todelim, todelim_include, concatParsers) where

import Data.Char
import Control.Monad
import Control.Applicative hiding (many)

infixr 5 +++

-- Monad of parsers: -------------------------------------------------

newtype Parser a = Parser (String -> [(a,String)])

instance Alternative Parser where
  empty = mzero
  (<|>) = mplus

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Monad Parser where
   return a      = Parser (\cs -> [(a,cs)])
   p >>= f       = Parser (\cs -> concat [parse (f a) cs' |
                                     (a,cs') <- parse p cs])

instance MonadPlus Parser where
   mzero          = Parser (\cs -> [])
   p `mplus` q    = Parser (\cs -> parse p cs ++ parse q cs)

-- Other parsing primitives: -----------------------------------------

parse           :: Parser a -> String -> [(a,String)]
parse (Parser p) = p

item            :: Parser Char
item             = Parser (\cs -> case cs of
                                     ""     -> []
                                     (c:cs) -> [(c,cs)])

sat             :: (Char -> Bool) -> Parser Char
sat p            = do {c <- item; if p c then return c else mzero}

-- Efficiency improving combinators: ---------------------------------

force           :: Parser a -> Parser a
force p          = Parser (\cs -> let xs = parse p cs in
                              (fst (head xs), snd (head xs)) : tail xs)

(+++)           :: Parser a -> Parser a -> Parser a
p +++ q          = Parser (\cs -> case parse (p `mplus` q) cs of
                                     []     -> []
                                     (x:xs) -> [x])

-- Recursion combinators: --------------------------------------------

string          :: String -> Parser String
string ""        = return ""
string (c:cs)    = do {char c; string cs; return (c:cs)}

many            :: Parser a -> Parser [a]
many p           = force (many1 p +++ return [])

many1           :: Parser a -> Parser [a]
many1 p          = do {a <- p; as <- many p; return (a:as)}

sepby           :: Parser a -> Parser b -> Parser [a]
p `sepby` sep    = (p `sepby1` sep) +++ return []

sepby1          :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep   = do {a <- p; as <- many (do {sep; p}); return (a:as)}

chainl          :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a    = (p `chainl1` op) +++ return a

chainl1         :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op   = do {a <- p; rest a}
                   where
                      rest a = do {f <- op; b <- p; rest (f a b)}
                               +++ return a

--"given a character will forcefully consume characters until it reaches the character " 
todelim c = many (sat (/= c))
todelim_include c = do {s <- todelim c; item; return $ (s ++ [c]) }


concatParsers ps = token $ Prelude.foldr (+++) (head ps) (tail ps)


-- Useful parsers: ---------------------------------------------------

char            :: Char -> Parser Char
char c           = sat (c ==)

digit           :: Parser Int
digit            = do {c <- sat isDigit; return (ord c - ord '0')}

lower           :: Parser Char
lower            = sat isLower

upper           :: Parser Char
upper            = sat isUpper

letter          :: Parser Char
letter           = sat isAlpha

alphanum        :: Parser Char
alphanum         = sat isAlphaNum

symb            :: String -> Parser String
symb cs          = token (string cs)

ident           :: [String] -> Parser String
ident css        = do cs <- token identifier
                      guard (not (elem cs css))
                      return cs

identifier      :: Parser String
identifier       = do {c <- lower; cs <- many alphanum; return (c:cs)}

nat             :: Parser Int
nat              = token natural

natural         :: Parser Int
natural          = digit `chainl1` return (\m n -> 10*m + n)

int             :: Parser Int
int              = token integer

integer         :: Parser Int
integer          = do {char '-'; n <- natural; return (-n)} +++ nat

-- Lexical combinators: ----------------------------------------------

space           :: Parser String
space            = many (sat isSpace)

token           :: Parser a -> Parser a
token p          = do {a <- p; space; return a}

apply           :: Parser a -> String -> [(a,String)]
apply p          = parse (do {space; p})

-- Example parser for arithmetic expressions: ------------------------
--
-- expr  :: Parser Int
-- addop :: Parser (Int -> Int -> Int)
-- mulop :: Parser (Int -> Int -> Int)
--
-- expr   = term   `chainl1` addop
-- term   = factor `chainl1` mulop
-- factor = token digit +++ do {symb "("; n <- expr; symb ")"; return n}
--
-- addop  = do {symb "+"; return (+)} +++ do {symb "-"; return (-)}
-- mulop  = do {symb "*"; return (*)} +++ do {symb "/"; return (div)}
--
----------------------------------------------------------------------
