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
import Data.List hiding (lookup)
import Data.Array()
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import System.Exit
import Parser
import BasicTypes
import Data.Tuple
import Prelude hiding (lookup)

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

parse_lines' lines = [let p_line = (lmap,((fst . head) stment))
                            where stment   = parse statement (unparsed ls)
                                  lmap = (line_num ls,n)
                      in p_line | (n,ls) <- zip [1..] (sort (tupled_lines lines))]

create_environment content table  = Program table program (fromList lm) (fromList f_n) (fromList $ fmap swap f_n)
  where (lm,sa) = unzip $ parse_lines' (lines content)
        program = array (1,length sa) [x | x <- zip [1..] sa]
        list_prog = assocs program
        f_n = [(i,find_next v list_prog) | (i,(FOR v _ _)) <- list_prog]

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


create_program_array content = (sorted_lines,array bound [(ix ls, ls) | ls <- sorted_lines])
                               where sorted_lines = parse_lines $ tupled_lines (lines content)
                                     bound = (1, length sorted_lines)

find_next _ [] = 0
find_next var ((i,s):rest) = case s of
                               NEXT var -> i
                               _ -> find_next var rest

-----------------------------------------------------------------------
--------------------- Evaluate Expression types -----------------------
-----------------------------------------------------------------------

eval_comp_expr arr e = (runReaderT $ eval_comp_expr' e) arr

eval_comp_expr' e = do
  tab <- ask
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

         FxnExpr "INT" e' -> do
           frac <- liftIO $ r e'
           return ((fromIntegral . floor) frac)

         FxnExpr "RND" e' -> do
           frac <- liftIO $ r e'
           if frac > 1
             then do
             rand <- liftIO $ (randomRIO (0,frac))
             return $ (fromIntegral.floor) rand
             else do
             rand <- liftIO $ randomRIO (0::Float,1::Float)
             return rand

         (Var v) -> do
           constValue <- liftIO $ (readArray tab v)
           return $ (num constValue)

interpreter   :: Int -> ReaderT Environment IO ()
interpreter n = do
  env@(Program tab program lines for_next next_for) <- ask
  let s = program ! n

  case s of
    (LET (Var i) e) -> do
      c <- liftIO (eval_expr env e)
      liftIO $ writeArray tab i (ConstExpr c)
      interpreter (n+1)

    (PRINT e) -> do
      e' <- liftIO (eval_expr env e)
      liftIO $ putStrLn . show $ e'
      interpreter (n+1)

    END -> liftIO $ exitWith ExitSuccess

    INPUT (Var c) -> do
      inp <- liftIO $ readLn
      liftIO $ writeArray tab c (ConstExpr inp)
      interpreter (n+1)


    FOR (Var c) e1 e2 -> do
      start <- liftIO $ (eval_expr env) e1
      finish <- liftIO $ (eval_expr env) e2
      liftIO $ writeArray tab c (ConstExpr start)
      if start >= finish
        then case (lookup n for_next) of
               Nothing -> do
                 liftIO $ putStrLn "NextNotFound"
                 interpreter (n + 1)
               Just l -> interpreter (l + 1)
        else interpreter (n + 1)


    NEXT (Var c) -> do
      let for_line = case (lookup n next_for) of
            Nothing -> n
            Just l -> l
      let (FOR _ e1 e2) = program ! for_line

      finish <- liftIO $ (eval_expr env) e2
      (ConstExpr value) <- liftIO $ readArray tab c
      
      if value < finish
        then do
        liftIO $ writeArray tab c (ConstExpr (value + 1))
        interpreter ( for_line + 1)
        else interpreter ( n + 1)


    IF compExp e -> do
      bool <- eval_comp_expr env compExp
      org_line <- liftIO $ eval_expr env e
      let next_line = case (lookup (round org_line) lines) of
            Nothing -> n
            Just l -> l
      if bool then interpreter next_line else interpreter (n + 1)

    _ -> do
      liftIO $ putStrLn "could not match"
      interpreter (n+1)


main = do
  --args <- getArgs
  --fileExists <- doesFileExist $ head args
  if True -------------------- fileExists __________________Changed for testing inside interactive GHCI without command line args
    then do handle <- openFile ("test.bas") ReadMode
            content <- hGetContents handle

            table <- newArray ('A','Z')
                     (ConstExpr 0) :: IO (IOArray Char Expression)
            let (Program tab sorted_ar linemap f_n n_f) =
                    create_environment content table

            putStrLn $ show f_n
            putStrLn $ show n_f
            putStrLn $ show linemap
            putStrLn $ show sorted_ar
    else do putStrLn $ "File " ++ ("") ++ " Does Not Exist."


-- ================================================================= --
--          Some convenience definitions for testing                 --
-- ================================================================= --


test_interp file = do
  handle <- openFile file ReadMode
  content <- hGetContents handle
  symbol_table <-
     newArray ('A','Z') (ConstExpr 0) :: IO (IOArray Char Expression)
  let env = create_environment content symbol_table
  putStrLn $ show env
  (runReaderT (interpreter 1)) env


test_parser file = do
  handle <- openFile file ReadMode
  content <- hGetContents handle
  let sorted_array = create_program_array content
  putStrLn $ show sorted_array


get_test_material file = do
  handle <- openFile file ReadMode
  content <- hGetContents handle
  table <- newArray ('A','Z') (ConstExpr 0) :: IO (IOArray Char Expression)
  let env = Program table program (fromList lm)
        where (lm,sa) = unzip $ parse_lines' (lines content)
              program = array (1,length sa) [x | x <- zip [1..] sa]
  return (env)



test_io_array = newArray ('A','Z')
                         (ConstExpr 0) :: IO (IOArray Char Expression)

test_expr1 = (MultExpr (ConstExpr 2)
                       (AddExpr (ConstExpr ( 3)) (ConstExpr ( 4.3))))
test_expr2 = (MultExpr ((Var 'A')) (AddExpr ((Var 'B')) ((Var 'C'))))
test_expr3 = (AddExpr ((Var 'A')) (AddExpr ((Var 'B')) ((Var 'C'))))

test_number_1 = ConstExpr ( 1)
test_number_5 = ConstExpr ( 5)
test_number_10 = ConstExpr ( 10)
test_var_x = Var 'X'
test_var_y = Var 'Y'
test_var_z = Var 'Z'
test_valuevar = VarVal test_var_x
test_valuefxn = FxnVal (RND (test_var_x))
-- test_valueconst = ConstVal ( 10)
test_expr_equals = CompEqualsExpr (test_var_x) (ConstExpr ( 10))
test_int_fxn_01 = INT (test_var_x)
test_int_fxn_02 = INT (AddExpr (test_var_x) (test_var_y))
test_int_fxn_03 = INT test_expr1
test_rnd_fxn_01 = RND (test_var_y)
test_rnd_fxn_02 = RND (AddExpr (test_var_x) (test_var_y))
test_rnd_fxn_03 = RND (test_expr1)

test_int_rnd_fxn_01 = FxnExpr "INT" (FxnExpr "RND" test_number_5)
test_int_rnd_fxn_02 =
  FxnExpr "INT" (MultExpr (FxnExpr "RND" test_number_5)
                          (AddExpr ((Var 'H')) test_number_1))
test_int_rnd_fxn_03 =
  FxnExpr "INT" (AddExpr (MultExpr (FxnExpr "RND" test_number_5) (Var 'H'))
                         (test_number_1))
test_statement_for = FOR test_var_x test_number_5 test_number_10
test_statement_forstep =
  FORSTEP test_var_x test_number_5 test_number_10 test_number_1
--test_statement_if = IF test_expr_equals ( 100)
test_statement_input = INPUT test_var_y
test_statement_let = LET test_var_x test_number_5
test_statement_next = NEXT test_var_x
test_statement_nextlist = NEXTLIST [test_var_x, test_var_y, test_var_z]
test_program         = "10 LET A = 2\n20 LET B = 3\n30 LET C = 4\n" ++
                       "40 PRINT A * (B + C)\n50 END"
test_program_fail    = "10 LET A = 2\n20 LET B = 3\nLET C = 4\n" ++
                       "40 PRINT A * (B + C)\n50 END"
test_program_list    = ["10 LET A = 2",
                        "20 LET B = 3",
                        "30 LET C = 4",
                        "40 PRINT A * (B + C)",
                        "50 END"]
test_program_list_02 = ["30 LET C = 4",
                        "20 LET B = 3",
                        "10 LET A = 2",
                        "40 PRINT A * (B + C)",
                        "50 END"]


-- eval_sttmnt       :: Statement -> Environment -> IO ()
-- eval_sttmnt s arr = (runReaderT $ eval_statement s) arr

-- eval_statement    :: Statement -> ReaderT Environment IO ()
-- eval_statement s = case s of
--                       (LET (Var i) e) -> do
--                         env <- ask
--                         c <- liftIO (eval_expr env e)
--                         liftIO $ writeArray (s_table env) i (ConstExpr c)
--                       (PRINT e) -> do
--                         env <- ask
--                         e' <- liftIO (eval_expr env e)
--                         liftIO $ putStrLn . show $ e'
--                       END -> liftIO $ exitWith ExitSuccess
--                       INPUT (Var c) -> do
--                         env <- ask
--                         inp <- liftIO $ readLn
--                         liftIO $ writeArray (s_table env) c (ConstExpr inp)
