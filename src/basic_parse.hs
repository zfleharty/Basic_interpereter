{----------------------------------------------------------------------}
{-                                                                    -}
{- CS 556: Adv Declarative Programming                                -}
{- Fall 2020                                                          -}
{- Project 03: BASIC parser                                           -}
{- Zach Fleharty & Hoss Craft                                         -}
{- submitted …                                                        -}
{-                                                                    -}
{----------------------------------------------------------------------}

{----------------------------------------------------------------------}
{- A Haskell-based implementation of parser for a smallish subset of
   the BASIC language.

   We need to be able to parse and interpret the following two simple
   BASIC programs:

   test.bas
   20 INPUT H
   25 LET X = INT(RND(1)*H+1)
   27 PRINT X
   30 FOR I = 1 TO H
   35 PRINT I
   40 IF I = X THEN 60
   50 NEXT I
   60 END

   foo.bas
   10 LET A = 2
   20 LET B = 3
   30 LET C = 4
   40 PRINT A * (B + C)
   50 END                                                             -}
{----------------------------------------------------------------------}
import System.IO
import Parselib
import Data.Char
import System.Environment
import Data.Ix()
import Data.Array
import System.Directory
import Data.List
import Data.Array()
import Data.IORef
import Control.Monad

data Statement      = LET Var Expression
                    | PRINT Expression
                    | END 

data Expression     = AddExp Expression Expression 
                    | MultExp Expression Expression
                    | Cons Constant
                    | Variable Var

data Var            = Var {character::Char}

data Constant       = Number Int | Symbol String

data Line_statement = Unparsed_line {line_num:: Int, unparsed:: String} 
                    | Parsed_line {line_num:: Int, sttment:: Statement} 


-------------------------------------------------------------------------------
-- state Monad implementation. May change later can not load in State Monad  --
-------------------------------------------------------------------------------


newtype State s a = State {runState :: s -> (a, s)}

instance Functor (State s) where
  fmap = liftM

instance Applicative (State s) where
  pure = return
  (<*>) = ap

instance Monad (State s) where
  return a = State $ \s -> (a, s)
  m >>= f = State $ \s->
    let (a', s') = runState m s
    in runState (f a') s'

get           = State $ \s -> (s, s)
put s         = State $ \_ -> ((), s)
evalState x s = fst $ runState x s


----------------------------------------------------------------------------------------
------------------------------Derived instances for Data types -------------------------
----------------------------------------------------------------------------------------

instance Show Expression where
  show (AddExp e1 e2)  = (show e1) ++ " + " ++ (show e2)
  show (MultExp e1 e2) = (show e1) ++ " * " ++ (show e2)
  show (Cons x)        = show x
  show (Variable x)    = show x

instance Show Var where
  show (Var x) = show x

instance Eq Line_statement where
  a == b = (line_num a) == (line_num b)

instance Ord Line_statement where
  compare a b = compare (line_num a) (line_num b)

instance Show Line_statement where
  show x = "(" ++ show (line_num x) ++ ", " ++ unparsed x ++ ")" 

instance Show Statement where                              
  show (LET x y) = "LET " ++ (show x) ++ " = " ++ (show y)
  show (PRINT e) = "PRINT " ++ show e
  show (END)     = "END"                                   

instance Show Constant where
  show (Number x) = show x
  show (Symbol x) = show (NoQuotes x)

newtype NoQuotes = NoQuotes String

instance Show NoQuotes where show (NoQuotes str) = str

-----------------------------_Main_----------------------------------------------

tuple_line         :: Parser Int
tuple_line         = do {num <- int; return num}

tupled_lines       :: [String] -> [Line_statement]
tupled_lines ls    = [let l_statement = Unparsed_line (fst tuple) (snd tuple)
                            where tuple = head (parse tuple_line x)
                                    in l_statement | x <- ls]

parse_lines lines = [let p_line = (n , (fst . head) stment)
                               where stment = parse statement (unparsed ls)
                               in p_line | (n,ls) <- zip [1..] (sort lines)]

---------------------------------------------------------------------------------------
-- Change to search for GoToStatements and renumber them according to the reordering --
---------------------------------------------------------------------------------------


--------------NEXT STEP------
--Attempting to implement a threaded symbol table somehow
--First trying to have a threaded symbol table of type list and eventually switch it to any Array of IORefs
--symbol_table = [(i,(Number 0)) | i <- ['A' ..'Z']]

-- writeToTable' var val = do tab <- get
--                           let newTab = [if (character var) == s then (s,val) else (s,v) | (s,v) <- tab]
--                           put newTab
--                           return tab

-- readFromTable' var = do tab <- get
--                        let varvar = (head) [v | (s,v) <- tab, s == (character var)]
--                        return (varvar)





writeToTable var val = do tab <- get
                          let ref = (tab ! var)
                          return $ writeIORef ref val
                              --return ()


readFromTable tab var = do ref <- (tab ! var)
                           return (readIORef ref)


runtest = (evalState $ test 'A') symbol_table
test var = do {tab <- get; writeToTable tab (var) (Number 3); variable <- readFromTable tab (var); return variable}
          

main = do
  args <- getArgs
  fileExists <- doesFileExist $ head args
  if fileExists 
    then do handle <- openFile (head args) ReadMode
            content <- hGetContents handle
            let sorted_array = array bound sorted_lines
                  where sorted_lines = parse_lines $ tupled_lines (lines content)
                        bound = (1, length sorted_lines)
                       
    
            putStrLn $ show sorted_array
            
    else do
    let symbol_table = array ('A','Z') [ (i,newIORef (Number 0))| i <- ['A'..'Z']]
    
    putStrLn $ "File " ++ (head args) ++ " Does Not Exist."
  
-- =========================================== --
--  Parsers for special individual characters  --
-- =========================================== --
equal = char ('=')
left  = char ('(')
right = char (')')
-- =========================================== --
--  Parsers for special key strings            --
-- =========================================== --
p_end :: Parser String
p_end = string "END"

p_let :: Parser String
p_let = string "LET"

p_print :: Parser String
p_print = string "PRINT"
-- =========================================== --
--  Parsers for particular Statements          --
-- =========================================== --

statement       :: Parser Statement
let_statement   :: Parser Statement
end_statement   :: Parser Statement
print_statement :: Parser Statement



statement     = let_statement +++ end_statement +++ print_statement

let_statement = do
  _ <- token p_let
  var <- token p_var
  _ <- token equal
  val <- token p_cons
  return (LET var (Cons val))
  
end_statement   = do {a <- token p_end; return END}

print_statement = do {token p_print; e <- expr; return (PRINT e)}

-- =========================================== --
--  Parsers (and supporting fxns) for          --
--  Statement components                       --
-- =========================================== --

p_cons   :: Parser Constant
p_number :: Parser Constant
p_var    :: Parser Var
p_symbol :: Parser Constant

var_char :: Char -> Bool
var_char_end :: Char -> Bool


var_char x     = isAlphaNum x || elem x "_"

var_char_end x = elem x "$%"


  
p_cons   = p_number +++ p_symbol

p_number = do {d <- token int; return (Number d)}

p_var    = do {var <- token upper; return (Var var)}

p_symbol = do {a <- sat (isAlpha); b <- many (sat var_char);
               c <- many (sat var_char_end); return (Symbol (a:(b++c)))}


var_expr :: Parser Expression
num_expr :: Parser Expression
expr     :: Parser Expression
value    :: Parser Expression
add_exp  :: Parser Expression
mult_exp :: Parser Expression




var_expr = do {var <- token upper; return (Variable (Var var))}

num_expr = do {d <- token int; return (Cons (Number d))}

expr     = add_exp +++ mult_exp

add_exp  = do {x <- token mult_exp; token (char '+');y <- token add_exp; return (AddExp x y)} +++ mult_exp

mult_exp = do {x <- token value; token (char '*'); y <- token mult_exp; return (MultExp x y)} +++ value

value    = do
  many (char '(')
  expr <- (num_expr) +++ (var_expr)
  many (char ')')
  return expr




  

-- ====================================== --
--  A Reminder of (some) of the Grammar   --
-- ====================================== --

{-
ID             = {letter}
String         = '"'{String Chars}*'"'
Integer        = {digit}+

<Statement>   ::= END
                | LET <Variable> '=' <Expression>
                | PRINT <Print list>
                | PRINT TAB '(' <Expression> ')' <Print list>

<Variable>    ::= ID
                | <Array>

-}


-- ========================================================= --
--  Some Notes on Variables and Dat Types In Chipmunk Basic  --
-- (see http://www.nicholson.com/rhn/basic/basic.man.html    --
-- ========================================================= --

{- VARIABLES and DATA TYPES

  Variable names can be up to 31 significant characters in
  length, starting with a letter, and optionally followed by
  letters, digits, underscores, or an ending dollar sign or
  percent character.  Variable names are case insensitive.
  Variables can hold floating point values (IEEE double),
  short integers, or strings of up to 254 characters.  If a
  variable name ends with a "$" character, it holds a string
  value, otherwise it holds numeric values.  If a variable
  name ends with the "%" character it may be limited to
  holding only integer values from -32768 to 32767.
-}

