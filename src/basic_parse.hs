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
import Data.Map hiding ((!),assocs)
import Parselib
import System.Random
import Data.Ix()
import Data.Array
import Data.List
import Data.Array()
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import System.Exit
import Parser
import BasicTypes
-- ================================== --
-- experimenting by Hoss              --
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
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

eval_comp_expr arr e = (runReaderT $ eval_comp_expr' e) arr

eval_comp_expr' e = do
  tab <- ask
  prog <- lift get
  case e of
    (CompEqualsExpr e1 e2) -> do
      v1 <- liftIO $ (eval_expr tab e1)
      v2 <- liftIO $ (eval_expr tab e2)
      return $ v1 == v2


eval_expr arr e = (runReaderT $ eval_expr' e) arr

eval_expr'   :: Expression -> ReaderT (Environment) IO (Float)
eval_expr' e = do
  env <- ask
  let tab = s_table env
  let r = (\e' -> (runReaderT $ eval_expr' e') env)
    in case e of

         AddExpr e1 e2 -> do
           i1 <- liftIO $ r e1
           i2 <- liftIO $ r e2
           return $ i1 + i2           

         MultExpr e1 e2 -> do
           i1 <- liftIO $ r e1
           i2 <- liftIO $ r e2
           return $ i1 * i2           

         ConstExpr c ->  return $ c

         FxnExpr (INT e') -> do
           frac <- liftIO $ r e'
           return ((fromIntegral .floor) frac)

         FxnExpr (RND e') -> do
           frac <- liftIO $ r e'
           if frac > 1             
             then do
             rand <- (randomRIO (0,frac))
             return $ (fromIntegral.floor) rand
             else do
             rand <- randomRIO (0::Float,1::Float)
             return rand

         (Var v) -> do
           constValue <- liftIO $ (readArray tab v)
           return $ (num constValue)


eval_sttmnt       :: Statement -> (Environment) -> IO ()
eval_sttmnt s arr = (runReaderT $ eval_statement s) arr             

eval_statement    :: Statement -> ReaderT (Environment) IO ()
eval_statement s= case s of                                        
                      (LET (Var i) e) -> do              
                        env <- ask
                        c <- liftIO (eval_expr env e)
                        liftIO $ writeArray (s_table env) i (ConstExpr c)                
                      (PRINT e) -> do                                
                        env <- ask
                        e' <- liftIO (eval_expr env e)
                        liftIO $ putStrLn . show $ e'
                      END -> liftIO $ exitWith ExitSuccess                
                      INPUT (Var c) -> do
                        env <- ask
                        inp <- liftIO $ readLn
                        liftIO $ writeArray (s_table env) c (ConstExpr inp)
                        
                                    
eval_line (Parsed_line i ol s)= do
  eval_sttmnt s

create_program_array content = array bound [(ix ls, ls) | ls <- sorted_lines]
                               where sorted_lines = parse_lines $ tupled_lines (lines content)
                                     bound = (1, length sorted_lines)


test_interp file = do
  handle <- openFile file ReadMode
  content <- hGetContents handle
  let sorted_array = create_program_array content
  symbol_table <- newArray ('A','Z') (ConstExpr 0) :: IO (IOArray Char Expression)
  sequence $ (`eval_line` (Program symbol_table)) <$> sorted_array


test_parser file = do
  handle <- openFile file ReadMode
  content <- hGetContents handle
  let sorted_array = create_program_array content
  putStrLn $ show sorted_array
  
main = do
  --args <- getArgs 
  --fileExists <- doesFileExist $ head args
  if True -------------------- fileExists __________________Changed for testing inside interactive GHCI without command line args
    then do handle <- openFile ("foo.bas") ReadMode
            content <- hGetContents handle
            let sorted_array = create_program_array content
            
            let line_table_array = fmap (\ls -> (origLine ls, ix ls)) sorted_array
            let line_table = let (min,max) = bounds line_table_array
                             in [line_table_array ! i | i <- [min..max] ]

            let line_map = fromList line_table
            putStrLn $ show line_map
    else do putStrLn $ "File " ++ ("") ++ " Does Not Exist."

  

-- ============================== --
--  some definitions for testing  --
-- ============================== --

test_io_array = newArray ('A','Z') (ConstExpr 0) :: IO (IOArray Char Expression)

test_expr1 = (MultExpr (ConstExpr 2) (AddExpr (ConstExpr ( 3)) (ConstExpr ( 4.3))))  
test_expr2 = (MultExpr ((Var 'A')) (AddExpr ((Var 'B')) ((Var 'C'))))
test_expr3 = (AddExpr ((Var 'A')) (AddExpr ((Var 'B')) ((Var 'C'))))

test_number_1 = ConstExpr ( 1)
test_number_5 = ConstExpr ( 5)
test_number_10 = ConstExpr ( 10)
test_var_x = Var 'X'
test_var_y = Var 'Y'
test_valuevar = VarVal test_var_x
-- test_valuefxn = FxnVal (RND (VarExpr test_var_x))
-- test_valueconst = ConstVal ( 10)
--test_expr_equals = CompEqualsExpr (VarExpr test_var_x) (ConstExpr ( 10))
test_int_fxn_01 = INT (test_var_x)
test_int_fxn_02 = INT (AddExpr (test_var_x) (test_var_y))
test_int_fxn_03 = INT test_expr1
test_rnd_fxn_01 = RND (test_var_y)
test_rnd_fxn_02 = RND (AddExpr (test_var_x) (test_var_y))
test_rnd_fxn_03 = RND (test_expr1)

test_int_rnd_fxn_01 = INT (FxnExpr (RND test_number_5))
test_int_rnd_fxn_02 = INT (MultExpr (FxnExpr (RND test_number_5)) (AddExpr ((Var 'H')) test_number_1))
test_int_rnd_fxn_03 = INT (AddExpr (MultExpr (FxnExpr (RND test_number_5)) ((Var 'H'))) (test_number_1))

test_statement_for = FOR test_var_x test_number_5 test_number_10
test_statement_forstep =
  FORSTEP test_var_x test_number_5 test_number_10 test_number_1
--test_statement_if = IF test_expr_equals ( 100)
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
