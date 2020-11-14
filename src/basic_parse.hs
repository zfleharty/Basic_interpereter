----------------------------------------------------------------------}
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
import Data.Array.IO
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
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import System.Exit
import Data.Maybe
-- ================================== --
-- experimenting by Hoss              --
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import BasicTypes
-- =================================== --

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
--parse_lines :: (Num a, Enum a) => [Line_statement] -> [(a, Statement)]
parse_lines lines = [let p_line = Parsed_line n  (line_num ls) ((fst . head) stment)
                               where stment = parse statement (unparsed ls)
                               in p_line | (n,ls) <- zip [1..] (sort lines)]


-----------------------------------------------------------------------------
--------------------- Evaluate Expression types -----------------------------
-----------------------------------------------------------------------------

eval_expr       :: Expression -> (IOArray Char Constant) -> IO Int
eval_expr e arr = (runReaderT $ eval_expr' e) arr
  

eval_expr'      :: Expression -> ReaderT (IOArray Char Constant) IO Int
eval_expr' e    = do
  tab <- ask
  let r = (\e' -> (runReaderT $ eval_expr' e') tab)
    in case e of
         AddExpr e1 e2 -> do
           i1 <- liftIO $ r e1
           i2 <- liftIO $ r e2
           return $ i1 + i2
         MultExpr e1 e2 -> do
           i1 <- liftIO $ r e1
           i2 <- liftIO $ r e2
           return $ i1 * i2
         ConstExpr c ->  return $ num c
         Variable (Var v) -> do
           constValue <- liftIO $ (readArray tab v)
           return $ (num constValue)
  


-----------------------------------------------------------------------------
--------------------- Evaluate Statement types -----------------------------
-----------------------------------------------------------------------------

eval_sttmnt       :: Statement -> (IOArray Char Constant) -> IO ()
eval_sttmnt s arr = (runReaderT $ eval_statement s) arr             

eval_statement    :: Statement -> ReaderT (IOArray Char Constant) IO ()
eval_statement s  = case s of                                        
                      (LET (Var i) e) -> do              
                        table <- ask
                        c <- liftIO (eval_expr e table)
                        liftIO $ writeArray table i (NumConst c)                
                      (PRINT e) -> do                                
                        table <- ask
                        e' <- liftIO (eval_expr e table)
                        liftIO $ putStrLn . show $ e'
                      END -> liftIO $ exitWith ExitSuccess                
                      INPUT (Var c) -> do
                        table <- ask
                        inp <- liftIO $ readLn
                        liftIO $ writeArray table c (NumConst inp)
              
                     
eval_line (Parsed_line i ol s) = do
  eval_sttmnt s 

create_program_array content = array bound [(ix ls, ls) | ls <- sorted_lines]
                               where sorted_lines = parse_lines $ tupled_lines (lines content)
                                     bound = (1, length sorted_lines)


testing file = do
  handle <- openFile file ReadMode
  content <- hGetContents handle
  let sorted_array = create_program_array content
  symbol_table <- newArray ('A','Z') (NumConst 0) :: IO (IOArray Char Constant)
  let inp_test = sorted_array ! 2
  putStrLn $ show inp_test
--  ((eval_line inp_test) symbol_table)
--  readArray symbol_table ('H')

test_interp :: String -> IO ()
test_interp file = do
  --file-handling
  handle <- openFile file ReadMode
  content <- hGetContents handle

  --init
  let sorted_array = create_program_array content
  symbol_table <- newArray ('A','Z') (NumConst 0) :: IO (IOArray Char Constant)

  --evaluation
  sequence $ (`eval_line` symbol_table) <$> sorted_array
  putStrLn "hello"
                

main = do
  --args <- getArgs 
  --fileExists <- doesFileExist $ head args
  if True -------------------- fileExists __________________Changed for testing inside interactive GHCI without command line args
    then do handle <- openFile ("foo.bas") ReadMode
            content <- hGetContents handle
            let sorted_array = array bound [(ix ls, ls) | ls <-sorted_lines]
                  where sorted_lines = parse_lines $ tupled_lines (lines content)
                        bound = (1, length sorted_lines)
            let symbol_table = [(i,(NumConst 0)) | i <- ['A' ..'Z']]

            let run_comp = (\f -> (evalState f) symbol_table)

            let first_statement = (sorted_array ! 1)

            -- experimenting here with a mutable array indexed by Chars
            symbol_array <- newArray ('A','Z') 0 :: IO (IOArray Char Int)
            a <- readArray symbol_array 'B'
            writeArray symbol_array 'B' 64
            b <- readArray symbol_array 'B'

            -- experimenting here with an immutable array for storing
            -- new vs. original line numbers
            let new_to_old_line_nums = array bound [(ix ls, origLine ls) | ls <- sorted_lines]
                  where sorted_lines = parse_lines $ tupled_lines (lines content)
                        bound = (1, length sorted_lines)

            -- clearly we can old line number from new:
            let old_line_num = new_to_old_line_nums!3

            -- can we (easily) get new line num from old?
            -- Because the original line numbers should be unique, we
            -- should be able to do something like this to find the new
            -- line number for the old line #30:
            -- let new_line_num = fst <$> find ((== 30) . snd) $ assocs new_to_old_line_nums
            let test_junk = fst <$> (find ((== 30) . snd) $ assocs new_to_old_line_nums)
            let new_line_num = fromJust test_junk

--            let value = run_comp $ (let_test first_statement)
            

  --          putStrLn $  show value

           --------------------------------------------------------------------------------------------------------------------------------
           --  let symbol_table = (array ('A','Z') [ (i,newIORef (NumConst 0))| i <- ['A'..'Z']])                                        --
           --  let f = (runReaderT $ (write_and_read_example 'A' (NumConst 3))) ::  Array Char (IO (IORef Constant)) -> IO (IO Constant) --
           --  value <- f symbol_table                                                                                                   --
           -- let symbol_table = [(i,(NumConst 0)) | i <- ['A' ..'Z']]                                                                   --
           -- let prgrm = Program symbol_table 1                                                                                         --
           --------------------------------------------------------------------------------------------------------------------------------
            
            putStrLn $ "The sorted_array is: "
            putStrLn $ show sorted_array
            putStrLn $ "1st statement is: " ++ (show first_statement)
            putStrLn $ "Example mutable array elems are: " ++ show (a, b)
            putStrLn $ "new_to_old_line_nums array is: " ++ (show (assocs new_to_old_line_nums))
            putStrLn $ "The 3rd old line num was: " ++ (show old_line_num)
            putStrLn $ "Index search result is: " ++ (show test_junk)
            putStrLn $ "30's new_line_num is: " ++ (show new_line_num)
            
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

p_int :: Parser String
p_int = string "INT"

p_rnd :: Parser String
p_rnd = string "RND"

-- at least one space
space1 :: Parser String
space1 = many1 (sat isSpace)

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
  for_statement +++ input_statement +++ if_statement +++
  let_statement +++ next_statement +++ print_statement +++
  end_statement
  

end_statement = do {_ <- token p_end; return END}

-- if_statement needs work: getting confused here with expressions
-- would be helpful to have a separate Boolean expression type
-- but our BASIC grammar doesn't seem to have that
-- Temporarily looking only for EqualsExpr in the IF
if_statement = do
  token p_if
  boolExpr <- token equals_expr
  token p_then
  c <- token p_const
  return (IF boolExpr c)

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
  return (LET var (ConstExpr val))

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

notAlphanum        :: Parser Char
notAlphanum         = sat (not.isAlphaNum)
  
p_const   = p_number +++ p_symbol

p_number = do {d <- token int; return (NumConst d)}

p_var    = do {var <- token upper; return (Var var)}
-- instead, make sure an upper case letter is followed by non-alpha char
-- to ensure we're only dealing with single-letter vars
-- p_var    = do {var <- upper; space1; return (Var var)}

p_symbol = do {a <- sat (isAlpha); b <- many (sat var_char);
               c <- many (sat var_char_end); return (StringConst (a:(b++c)))}


var_expr     :: Parser Expression
num_expr     :: Parser Expression
expr         :: Parser Expression
add_expr     :: Parser Expression
mult_expr    :: Parser Expression
int_fxn_expr :: Parser Expression
equals_expr  :: Parser CompareExpr

value        :: Parser Expression


var_expr = do {var <- token upper; return (Variable (Var var))}
-- instead, can we make sure an upper case letter is followed by a
-- non-alphanumeric character, so we don't end up consuming the
-- the first letters of functions like INT or RND?
-- to ensure we're only dealing with single-letter vars
-- var_expr = do {var <- upper; notAlphanum; return (Variable (Var var))}

num_expr = do {d <- token int; return (ConstExpr (NumConst d))}

-- here try looking for int_fxn_expr FIRST so that the 1st letter(s)
-- not mistaken for variables?
expr     = int_fxn_expr +++ add_expr +++ mult_expr +++ add_expr_paren 

add_expr  = do {
  x <- token mult_expr;
  token (char '+');
  y <- token add_expr;
  return (AddExpr x y)} +++ mult_expr

add_expr_paren = do {
  token (char '(');
  x <- token mult_expr;
  token (char '+');
  y <- token add_expr;
  token (char ')');
  return (AddExpr x y)} +++ mult_expr

mult_expr = do {
  x <- token value;
  token (char '*');
  y <- token mult_expr;
  return (MultExpr x y)} +++ value

int_fxn_expr = do {
  token p_int;
  token (char '(');
  e <- token expr;
  token (char ')');
  return (FxnExpr (INT e))
}

-- set up for Expression = Expression
-- may be too general for our needs and rely on inadeq expr parser
equals_expr = do
  x <- token expr
  token equal
  y <- token expr
  return (EqualsExpr x y)

value    = do{
  many1 (char '(');
  e <- expr;
  many1 (char ')');
  return e} +++  (num_expr) +++ (var_expr)


-- ============================== --
--  some definitions for testing  --
-- ============================== --

test_io_array = newArray ('A','Z') (NumConst 0) :: IO (IOArray Char Constant)

test_expr1 = (MultExpr (ConstExpr (NumConst 2)) (AddExpr (ConstExpr (NumConst 3)) (ConstExpr (NumConst 4))))  
test_expr2 = (MultExpr (Variable (Var 'A')) (AddExpr (Variable (Var 'B')) (Variable (Var 'C'))))
test_expr3 = (AddExpr (Variable (Var 'A')) (AddExpr (Variable (Var 'B')) (Variable (Var 'C'))))

test_number_1 = ConstExpr (NumConst 1)
test_number_5 = ConstExpr (NumConst 5)
test_number_10 = ConstExpr (NumConst 10)
test_conststring = StringConst "ABC"
test_var_x = Var 'X'
test_var_y = Var 'Y'
test_valuevar = ValueVar test_var_x
-- test_valuefxn = ValueFxn (RND (Variable test_var_x))
-- test_valueconst = ValueConst (NumConst 10)
--test_expr_equals = EqualsExpr (Variable test_var_x) (ConstExpr (NumConst 10))
test_int_fxn = INT (Variable test_var_x)
test_int_fxn_02 = INT (AddExpr (Variable test_var_x) (Variable test_var_y))
test_int_fxn_03 = INT test_expr1
test_rnd_fxn = RND (Variable test_var_y)
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
test_program_list_02 = ["30 LET C = 4",
                     "20 LET B = 3",
                     "10 LET A = 2",
                     "40 PRINT A * (B + C)",
                     "50 END"]

test_02 = do
  let lines = tupled_lines test_program_list_02
  putStrLn $ "lines = " ++ (show lines)
  let sorted_lines = sort lines
  putStrLn $ "sorted_lines = " ++ (show sorted_lines)
  let zipped_sorted_lines = zip [1..] sorted_lines
  putStrLn $ "zipped_sorted_lines = " ++ (show zipped_sorted_lines)
  let head_of_zipped = head zipped_sorted_lines
  putStrLn $ "head_of_zipped = " ++ (show head_of_zipped)
  let (n, ls) = head_of_zipped
  let stment = parse statement (unparsed ls)
  putStrLn $ "stment = " ++ (show stment)
  let str_of_stment = (fst . head) stment
  putStrLn $ "str_of_stment = " ++ (show str_of_stment)
  let parsed_line_construction = Parsed_line {ix=1, origLine=10, sttment = str_of_stment}
  putStrLn $ "parsed_line_construction = " ++ (show parsed_line_construction)

test_03 = do
  let symbol_array = array ('A', 'Z') [(c, 0) | c <- ['A'..'Z']]
  putStrLn $ show symbol_array
  -- modify the entry for 'B' from 0 to 3:
  arr <- (return $ symbol_array // [('B', 3)])
  -- let symbol_array = symbol_array // [('B', 3)]
  putStrLn $ show arr
  -- putStrLn $ show symbol_array

test_04 = do
  -- arr <- newArray (1,10) 37 :: IO (IOArray Int Int)
  arr <- newArray ('A','Z') 0 :: IO (IOArray Char Int)
  -- a <- readArray arr 1
  a <- readArray arr 'B'
  -- writeArray arr 1 64
  writeArray arr 'B' 1 
  -- b <- readArray arr 1
  b <- readArray arr 'B'
  
  print (a,b)
  -- putStrLn $ show arr
  

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

