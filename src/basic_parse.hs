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
import Data.Char
import System.Environment
import System.Random
import Data.Ix()
import Data.Array
import System.Directory
import Data.List
import Data.Array()
import Data.IORef
import Control.Monad.Trans
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import System.Exit
import Data.Maybe
import Parser
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

eval_expr       :: Expression -> (IOArray Char Constant) -> IO (Float)
eval_expr e arr = (runReaderT $ eval_expr' e) arr




eval_expr'      :: Expression -> ReaderT (IOArray Char Constant) IO (Float)
eval_expr' e = do
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
         FxnExpr (INT e') -> do
           frac <- liftIO $ r e'
           return ((fromIntegral .floor) frac)
         FxnExpr (RND e') -> do
           frac <- liftIO $ r e'
           let gen = mkStdGen (10)
           if frac > 1             
             then do
             let (result,_) = uniformR (0, (frac - 1)) gen 
             return (fromIntegral.ceiling $ result)
             else do
             let (result,_) = uniformR (0::Float,1::Float) gen
             return result
         VarExpr (Var v) -> do
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
  -- might the symbol table need to hold epxressions more generally
  -- rather than just NumConst's?
  symbol_table <- newArray ('A','Z') (NumConst 0) :: IO (IOArray Char Constant)
  putStrLn $ "Entire sorted_array is: " ++ (show (assocs sorted_array))
  let inp_test = sorted_array ! 2
  putStrLn $ "Example item from sorted_array: " ++ show inp_test
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
            let sorted_array = create_program_array content
            
            let line_table_array = fmap (\ls -> (origLine ls, ix ls)) sorted_array
            let line_table = let (min,max) = bounds line_table_array
                             in [line_table_array ! i | i <- [min..max] ]

            let line_map = fromList line_table
            putStrLn $ show line_map
    else do putStrLn $ "File " ++ ("") ++ " Does Not Exist."

-- ================================== --
-- experimenting by Hoss              --
-- main2 = do
--     ls <- fmap Text.lines (Text.readFile "foo.bas")
--     do 
--       let ls' = map Text.unpack ls
--       putStrLn $ show ls'
-- =================================== --



  

-- ============================== --
--  some definitions for testing  --
-- ============================== --

test_io_array = newArray ('A','Z') (NumConst 0) :: IO (IOArray Char Constant)

test_expr1 = (MultExpr (ConstExpr (NumConst 2)) (AddExpr (ConstExpr (NumConst 3)) (ConstExpr (NumConst 4.3))))  
test_expr2 = (MultExpr (VarExpr (Var 'A')) (AddExpr (VarExpr (Var 'B')) (VarExpr (Var 'C'))))
test_expr3 = (AddExpr (VarExpr (Var 'A')) (AddExpr (VarExpr (Var 'B')) (VarExpr (Var 'C'))))

test_number_1 = ConstExpr (NumConst 1)
test_number_5 = ConstExpr (NumConst 5)
test_number_10 = ConstExpr (NumConst 10)
test_conststring = StringConst "ABC"
test_var_x = Var 'X'
test_var_y = Var 'Y'
test_valuevar = VarVal test_var_x
-- test_valuefxn = FxnVal (RND (VarExpr test_var_x))
-- test_valueconst = ConstVal (NumConst 10)
--test_expr_equals = CompEqualsExpr (VarExpr test_var_x) (ConstExpr (NumConst 10))
test_int_fxn_01 = INT (VarExpr test_var_x)
test_int_fxn_02 = INT (AddExpr (VarExpr test_var_x) (VarExpr test_var_y))
test_int_fxn_03 = INT test_expr1
test_rnd_fxn_01 = RND (VarExpr test_var_y)
test_rnd_fxn_02 = RND (AddExpr (VarExpr test_var_x) (VarExpr test_var_y))
test_rnd_fxn_03 = RND (test_expr1)

test_int_rnd_fxn_01 = INT (FxnExpr (RND test_number_5))
test_int_rnd_fxn_02 = INT (MultExpr (FxnExpr (RND test_number_5)) (AddExpr (VarExpr (Var 'H')) test_number_1))
test_int_rnd_fxn_03 = INT (AddExpr (MultExpr (FxnExpr (RND test_number_5)) (VarExpr (Var 'H'))) (test_number_1))

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



eval_expr2 arr prog e = (runStateT $ (runReaderT $ eval_expr2' e) arr) prog

eval_expr2'   :: Expression -> ReaderT (IOArray Char Constant) (StateT Program IO) (Float)
eval_expr2' e = do
  tab <- ask
  prog <- lift get
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
         FxnExpr (INT e') -> do
           frac <- liftIO $ r e'
           return ((fromIntegral .floor) frac)
         FxnExpr (RND e') -> do
           frac <- liftIO $ r e'
           let sgen = gen prog
           if frac > 1             
             then do
             let (result,newGen) = uniformR (0, (frac - 1)) sgen
             lift $ put (ProgInfo newGen)
             return (fromIntegral.ceiling $ result)
             else do
             let (result,newGen) = uniformR (0::Float,1::Float) sgen
             lift $ put (ProgInfo newGen)
             return result
         VarExpr (Var v) -> do
           constValue <- liftIO $ (readArray tab v)
           return $ (num constValue)

test_eval_expr e = do
  arr <- test_io_array
  let p = ProgInfo (mkStdGen 10)
  eval_expr2 arr p e


test_rnd = do
  let e = FxnExpr . RND . ConstExpr . NumConst $ 10
  arr <- test_io_array
  let p = ProgInfo (mkStdGen 400)
  (v1,prog1) <- eval_expr2 arr p e
  (v2,prog2) <- eval_expr2 arr prog1 e
  (v3,prog3) <- eval_expr2 arr prog2 e
  return $ v1:v2:v3:[]
  
  
  
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

test_05 =  do
  putStrLn ""
  putStrLn $ "test_int_fxn_01: " ++ (show test_int_fxn_01)
  let parsedExpr = parse expr (show test_int_fxn_01)
  putStrLn $ "parse expr test_int_fxn_01: " ++ (show parsedExpr)
  putStrLn ""
  putStrLn $ "test_int_fxn_02: " ++ (show test_int_fxn_02)
  let parsedExpr = parse expr (show test_int_fxn_02)
  putStrLn $ "parse expr test_int_fxn_02: " ++ (show parsedExpr)
  putStrLn ""
  putStrLn $ "test_int_fxn_03: " ++ (show test_int_fxn_03)
  let parsedExpr = parse expr (show test_int_fxn_03)
  putStrLn $ "parse expr test_int_fxn_03: " ++ (show parsedExpr)
  putStrLn ""
  putStrLn $ "test_rnd_fxn_01: " ++ (show test_rnd_fxn_01)
  let parsedExpr = parse expr (show test_rnd_fxn_01)
  putStrLn $ "parse expr test_rnd_fxn_01: " ++ (show parsedExpr)
  putStrLn ""
  putStrLn $ "test_rnd_fxn_02: " ++ (show test_rnd_fxn_02)
  let parsedExpr = parse expr (show test_rnd_fxn_02)
  putStrLn $ "parse expr test_rnd_fxn_02: " ++ (show parsedExpr)
  putStrLn ""
  putStrLn $ "test_rnd_fxn_03: " ++ (show test_rnd_fxn_03)
  let parsedExpr = parse expr (show test_rnd_fxn_03)
  putStrLn $ "parse expr test_rnd_fxn_03: " ++ (show parsedExpr)
  putStrLn ""
  putStrLn $ "test_int_rnd_fxn_01: " ++ (show test_int_rnd_fxn_01)
  let parsedExpr = parse expr (show test_int_rnd_fxn_01)
  putStrLn $ "parse expr test_int_rnd_fxn_01: " ++ (show parsedExpr)
  putStrLn ""
  putStrLn $ "test_int_rnd_fxn_02: " ++ (show test_int_rnd_fxn_02)
  let parsedExpr = parse expr (show test_int_rnd_fxn_02)
  putStrLn $ "parse expr test_int_rnd_fxn_02: " ++ (show parsedExpr)
  putStrLn ""
  putStrLn $ "test_int_rnd_fxn_03: " ++ (show test_int_rnd_fxn_03)
  let parsedExpr = parse expr (show test_int_rnd_fxn_03)
  putStrLn $ "parse expr test_int_rnd_fxn_03: " ++ (show parsedExpr)
  putStrLn ""






run_test = do                                                    --
  let arr = array ('A','Z') [(x,(NumConst 2)) | x <- ['A'..'Z']] --
  let gen = ProgInfo (mkStdGen 10)                               --
  f <- (runReaderT $ ((runStateT (testing_fxn2 'A')) gen)) arr
  return f


testing_fxn2 :: Char -> StateT Program (ReaderT (Array Char Constant) IO) Float
testing_fxn2 e = do
  prog <- get
  table <- lift ask
  let r = num (table ! e)
  let (value,newGen) = uniformR (0,r) (gen prog)
  put (ProgInfo newGen)
  return $ value


testing_fxn :: Char -> ReaderT (Array Char Constant) (StateT Program IO) Float
testing_fxn e = do
  table <- ask
  prog <- lift get
  let r = num (table ! e)
  let (value,newGen) = uniformR (0,r) (gen prog)
--  lift $ put $ ProgInfo newGen
  return $ value


-- tests of statement parsing
test_06 = do
  putStrLn ""
  putStrLn " TESTS OF STATEMENT PARSING "
  putStrLn "============================"
  putStrLn ""
  let parsedExpr = parse statement "INPUT H"
  putStrLn $ "parse statement \"INPUT H\": " ++ (show parsedExpr)
  putStrLn ""
  let parsedExpr = parse statement "LET X = INT(RND(5)*H+1)"
  putStrLn $ "parse statement \"LET X = INT(RND(5)*H+1)\": " ++ 
             (show parsedExpr)
  putStrLn ""
  let parsedExpr = parse statement "PRINT X"
  putStrLn $ "parse statement \"PRINT X\": " ++ (show parsedExpr)
  putStrLn ""
  let parsedExpr = parse statement "FOR I = 1 TO H"
  putStrLn $ "parse statement \"FOR I = 1 TO H\": " ++ (show parsedExpr)
  putStrLn ""
  let parsedExpr = parse statement "PRINT I"
  putStrLn $ "parse statement \"PRINT I\": " ++ (show parsedExpr)
  putStrLn ""
  let parsedExpr = parse statement "IF I = X THEN 60"
  putStrLn $ "parse statement \"IF I = X THEN 60\": " ++ (show parsedExpr)
  putStrLn ""
  let parsedExpr = parse statement "NEXT I"
  putStrLn $ "parse statement \"NEXT I\": " ++ (show parsedExpr)
  putStrLn ""
  let parsedExpr = parse statement "END"
  putStrLn $ "parse statement \"END\": " ++ (show parsedExpr)


