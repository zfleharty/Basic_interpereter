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

-- ================================== --
-- experimenting by Hoss              --
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
-- =================================== --

data Statement      = FOR Var Expression Expression
                    | FORSTEP Var Expression Expression Expression
                    | IF Expression Constant
                    | INPUT Var
                    | LET Var Expression
                    | NEXT Var
                    | PRINT Expression
                    | END 




-- class Interpreter a b where
--   eval :: a -> b

--class EvNumExpr a where
--   eval :: a -> Int

-- instance EvNumExpr Expression Int where
--   eval (AddExp e1 e2) = (eval e1) + (eval e2)
--   eval (MultExp e1 e2) = (eval e1) * (eval e2)
--   eval (ConstExp e) = num e
--   eval (Varabile v) = do {ref <- get_Ref (character v); value <- readIORef ref; return }



data Expression     = AddExp Expression Expression 
                    | MultExp Expression Expression
--                    | EqualsExp Expression Expression
                    | ConstExp {cons::Constant}
                    | Variable {var::Var}

data CompareExpr    = EqualsExpr Expression Expression


data Var            = Var {character::Char}

data Constant       = NumConst {num::Int}
                    | StringConst {str::String}

data NegateExp      = Neg PowerExp
                    | Pexp PowerExp -- fix this!

data PowerExp      = Pow Value PowerExp
                    | PowValue Value

data Value          = ValueParens Expression
                    | ValueVar Var
                    | ValueFxn Function
                    | ValueConst Constant

data Function       = INT Expression
                    | RND Expression

data Line_statement = Unparsed_line {line_num:: Int, unparsed:: String} 
                    | Parsed_line {line_num:: Int, sttment:: Statement} 

data Interpereter = Program {s_table :: Array Char (IO (IORef Constant)), program_counter:: Int}
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


-------------------------------------------------------------
-- Derived instances for Data types                        --
-------------------------------------------------------------

instance Show Expression where
  show (AddExp e1 e2)  = (show e1) ++ " + " ++ (show e2)
  show (MultExp e1 e2) = (show e1) ++ " * " ++ (show e2)
--  show (EqualsExp e1 e2)  = (show e1) ++ " = " ++ (show e2)
  show (ConstExp x)       = show x
  show (Variable x)    = show x

instance Show Var where
  show (Var x) = show (NoQuotesChar x)

instance Eq Line_statement where
  a == b = (line_num a) == (line_num b)

instance Ord Line_statement where
  compare a b = compare (line_num a) (line_num b)

instance Show Line_statement where
  show x = "(" ++ show (line_num x) ++ ", " ++ unparsed x ++ ")" 

instance Show Statement where   
  show (FOR x e1 e2) =
    "FOR " ++ (show x) ++ " = " ++ (show e1) ++ " TO " ++ (show e2)
  show (FORSTEP x e1 e2 e3) =
    "FOR " ++ (show x) ++ " = " ++ (show e1)
    ++ " TO " ++ (show e2) ++ " STEP " ++ (show e3)
  show (IF e x)   = "IF " ++ (show e) ++ " THEN " ++ (show x)
  show (INPUT x)  = "INPUT " ++ (show x)
  show (LET x y)  = "LET " ++ (show x) ++ " = " ++ (show y)
  show (NEXT x)   = "NEXT " ++ (show x)
  show (PRINT e)  = "PRINT " ++ show e
  show (END)      = "END"                                   

instance Show Constant where
  show (NumConst x) = show x
  show (StringConst x) = show (NoQuotes x)

instance Show Value where
  show (ValueVar x)   = show x
  -- show (ValueFxn x)   = show x
  -- show (ValueConst x) = show x

instance Show Function where
  show (INT e)    = "INT(" ++ (show e) ++ ")"
  show (RND e)    = "RND(" ++ (show e) ++ ")"

newtype NoQuotes = NoQuotes String
newtype NoQuotesChar = NoQuotesChar Char

instance Show NoQuotes where show (NoQuotes str) = str
instance Show NoQuotesChar where show (NoQuotesChar char) = show (NoQuotes [char])

-----------------------------_Main_-------------------------------------

tuple_line :: Parser Int
tuple_line = do {num <- int; return num}

-- tupled_lines     Processes a list of strings, where each string
--                  represents a line from a BASIC program, producing a
--                  list of tuple-like Unparsed_Line constructions
--                  which print out like (line #, statement)
tupled_lines :: [String] -> [Line_statement]
tupled_lines ls = [let l_statement = Unparsed_line (fst tuple) (snd tuple)
                            where tuple = head (parse tuple_line x)
                                    in l_statement | x <- ls]

-- parse_lines      Processes a list of Line_Statements, typically
--                  produced by the tupled_lines fxn and which appear
--                  as tuples of the form (line #, Line_statement),
--                  returning a list of tuples of the form
--                  (new line #, Statement) where the new line #s now
--                  run consecutively 1, 2, 3, etc.
parse_lines :: (Num a, Enum a) => [Line_statement] -> [(a, Statement)]
parse_lines lines = [let p_line = (n , (fst . head) stment)
                               where stment = parse statement (unparsed ls)
                               in p_line | (n,ls) <- zip [1..] (sort lines)]


------------------------------------------------------------------------
-- Change to search for GoToStatements and renumber them according to --
-- the reordering                                                     --
------------------------------------------------------------------------


--------------NEXT STEP------
--Attempting to implement a threaded symbol table somehow
--First trying to have a threaded symbol table of type list and eventually switch it to any Array of IORefs



symbol_table = [(i,(NumConst 0)) | i <- ['A' ..'Z']]

edit_table :: [(Char,Constant)] -> Char -> Constant -> [(Char,Constant)]
edit_table [] _ _ = []
edit_table ((s,v):rest) sym val = if sym == s
  then (s,val) : rest
  else (s,v) : (edit_table rest sym val)



read_table ((_,v):[]) _ = v
read_table ((s,v):rest) sym = if sym == s then v else read_table rest sym

write_to_table var val = do tab <- get
                            put (edit_table tab var val)
                            return ()

read_from_table var = do tab <- get
                         return (read_table tab var)


test = (evalState $ eval_test 'A' (NumConst 43)) symbol_table
eval_test var val = do {write_to_table 'A' val; nv <- read_from_table var; return nv}
-------------------------------------------------------------------------------------------------------------
-- writeToTable var val = do tab <- get                                                                    --
--                           let newTab = [if (character var) == s then (s,val) else (s,v) | (s,v) <- tab] --
--                           put tab                                                                       --
--                           return tab                                                                    --
--                                                                                                         --
-- readFromTable' var = do tab <- get                                                                      --
--                        let varvar = (head) [v | (s,v) <- tab, s == (character var)]                     --
--                        return (varvar)                                                                  --
--                                                                                                         --
-- readRefFromTable :: Char -> State Interpereter (IO (IORef Constant))                                    --
-- readRefFromTable var = do prgrm_info <- get                                                             --
--                           return ((s_table prgrm_info) ! var)                                           --
--                                                                                                         --
-- get_ref :: Char -> Interpereter -> IO (IORef Constant)                                                  --
-- get_ref var = (evalState $ readRefFromTable var)                                                        --
-------------------------------------------------------------------------------------------------------------




main = do
  --args <- getArgs 
  --fileExists <- doesFileExist $ head args
  if True -------------------- fileExists __________________Changed for testing inside interactive GHCI without command line args
    then do handle <- openFile ("foo.bas") ReadMode
            content <- hGetContents handle
            let sorted_array = array bound sorted_lines
                  where sorted_lines = parse_lines $ tupled_lines (lines content)
                        bound = (1, length sorted_lines)
            let symbol_table = (array ('A','Z') [ (i,newIORef (NumConst 0))| i <- ['A'..'Z']])
            let prgrm = Program symbol_table 1
            putStrLn $ show sorted_array
            
    else do putStrLn $ "File " ++ ("") ++ " Does Not Exist."

-- ================================== --
-- experimenting by Hoss              --
main2 = do
    ls <- fmap Text.lines (Text.readFile "foo.bas")
    do 
      let ls' = map Text.unpack ls
      putStrLn $ show ls'
-- =================================== --



  
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

p_for :: Parser String
p_for = string "FOR"

p_if :: Parser String
p_if = string "IF"

p_input :: Parser String
p_input = string "INPUT"

p_let :: Parser String
p_let = string "LET"

p_next :: Parser String
p_next = string "NEXT"

p_print :: Parser String
p_print = string "PRINT"

p_then :: Parser String
p_then = string "THEN"

p_to :: Parser String
p_to = string "TO"

-- =========================================== --
--  Parsers for particular Statements          --
-- =========================================== --

statement       :: Parser Statement
end_statement   :: Parser Statement
-- for_statement   :: Parser Statement
--if_statement    :: Parser Statement
input_statement :: Parser Statement
let_statement   :: Parser Statement
-- next_statement  :: Parser Statement
print_statement :: Parser Statement

statement =
  for_statement +++ input_statement +++ --if_statement
  let_statement +++ next_statement +++ print_statement +++
  end_statement
  

end_statement = do {_ <- token p_end; return END}

-- if_statement needs work: getting confused here with expressions
-- would be helpful to have a separate Boolean expression type
-- but our BASIC grammar doesn't seem to have that
-- Temporarily looking only for EqualsExp in the IF
-- if_statement = do
--   token p_if
--   boolExpr <- token equals_expr
--   token p_then
--   c <- token p_const
--   return (IF boolExpr c)

input_statement = do
  token p_input
  var <- token p_var
  return (INPUT var)

for_statement = do
  token p_for
  var <- token p_var
  token equal
  fromExpr <- token expr
  token p_to
  toExpr <- token expr
  return (FOR var fromExpr toExpr)

let_statement = do
  _ <- token p_let
  var <- token p_var
  _ <- token equal
  val <- token p_const
  return (LET var (ConstExp val))

next_statement = do
  token p_next
  var <- token p_var
  return (NEXT var)

print_statement = do {token p_print; e <- expr; return (PRINT e)}

-- =========================================== --
--  Parsers (and supporting fxns) for          --
--  Statement components                       --
-- =========================================== --

p_const  :: Parser Constant
p_number :: Parser Constant
p_var    :: Parser Var
p_symbol :: Parser Constant

var_char :: Char -> Bool
var_char_end :: Char -> Bool


var_char x     = isAlphaNum x || elem x "_"

var_char_end x = elem x "$%"


  
p_const   = p_number +++ p_symbol

p_number = do {d <- token int; return (NumConst d)}

p_var    = do {var <- token upper; return (Var var)}

p_symbol = do {a <- sat (isAlpha); b <- many (sat var_char);
               c <- many (sat var_char_end); return (StringConst (a:(b++c)))}


var_expr    :: Parser Expression
num_expr    :: Parser Expression
expr        :: Parser Expression
add_exp     :: Parser Expression
mult_exp    :: Parser Expression
--equals_expr :: Parser Expression
value       :: Parser Expression


var_expr = do {var <- token upper; return (Variable (Var var))}

num_expr = do {d <- token int; return (ConstExp (NumConst d))}

expr     = add_exp +++ mult_exp

add_exp  = do {
  x <- token mult_exp;
  token (char '+');
  y <- token add_exp;
  return (AddExp x y)} +++ mult_exp

mult_exp = do {
  x <- token value;
  token (char '*');
  y <- token mult_exp;
  return (MultExp x y)} +++ value

-- set up for Expression = Expression
-- may be too general for our needs and rely on inadeq expr parser
-- equals_expr = do
--   x <- token expr
--   token equal
--   y <- token expr
--   return (EqualsExp x y)

value    = do
  many (char '(')
  expr <- (num_expr) +++ (var_expr)
  many (char ')')
  return expr


-- ============================== --
--  some definitions for testing  --
-- ============================== --

test_number_1 = ConstExp (NumConst 1)
test_number_5 = ConstExp (NumConst 5)
test_number_10 = ConstExp (NumConst 10)
test_conststring = StringConst "ABC"
test_var_x = Var 'X'
test_var_y = Var 'Y'
test_valuevar = ValueVar test_var_x
-- test_valuefxn = ValueFxn (RND (Variable test_var_x))
-- test_valueconst = ValueConst (NumConst 10)
--test_expr_equals = EqualsExp (Variable test_var_x) (ConstExp (NumConst 10))
test_statement_for = FOR test_var_x test_number_5 test_number_10
test_statement_forstep =
  FORSTEP test_var_x test_number_5 test_number_10 test_number_1
--test_statement_if = IF test_expr_equals (NumConst 100)
test_statement_input = INPUT test_var_y
test_statement_let = LET test_var_x test_number_5
test_statement_next = NEXT test_var_x
test_program = "10 LET A = 2\n20 LET B = 3\n30 LET C = 4\n40 PRINT A * (B + C)\n50 END"
test_program_fail = "10 LET A = 2\n20 LET B = 3\nLET C = 4\n40 PRINT A * (B + C)\n50 END"
test_program_list = ["10 LET A = 2",
                     "20 LET B = 3",
                     "30 LET C = 4",
                     "40 PRINT A * (B + C)",
                     "50 END"]

  

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

