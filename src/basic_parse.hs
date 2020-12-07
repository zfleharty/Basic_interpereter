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
import Data.Map hiding ((!),assocs,split)
import Parselib
import System.Random hiding (split)
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
import Prelude hiding (lookup,LT,GT)


split d [] = []
split d cs = x : split d (Prelude.drop 1 y) where (x,y) = span (/= d) cs

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

parse_lines'' lines = [let p_line = (lmap,((fst . head) stment))
                            where stment   = parse statement (unparsed ls)
                                  lmap = (line_num ls,n)
                      in p_line | (n,ls) <- zip [1..] ((tupled_lines' lines))]


create_environment'' content table  = Program table program (fromList lm) (fromList f_n) (fromList $ fmap swap f_n)
  where (lm,sa) = unzip $ parse_lines'' (content)
        program = array (1,length sa) [x | x <- zip [1..] sa]
        list_prog = assocs program
        f_n = [(i,find_next v list_prog) | (i,(FOR v _ _)) <- list_prog]

parse_lines lines = [let p_line = (lmap,((fst . head) stment))
                            where stment   = parse statement (unparsed ls)
                                  lmap = (line_num ls,n)
                      in p_line | (n,ls) <- zip [1..] (sort (tupled_lines lines))]

create_environment content table  = Program table program (fromList lm) (fromList f_n) (fromList $ fmap swap f_n)
  where (lm,sa) = unzip $ parse_lines (lines content)
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
parse_lines' lines = [let p_line = Parsed_line n  (line_num ls) ((fst . head) stment)
                               where stment = parse statement (unparsed ls)
                               in p_line | (n,ls) <- zip [1..] (sort lines)]


create_program_array content = (sorted_lines,array bound [(ix ls, ls) | ls <- sorted_lines])
                               where sorted_lines = parse_lines' $ tupled_lines (lines content)
                                     bound = (1, length sorted_lines)

find_next _ [] = 0
find_next var ((i,s):rest) = case s of
                               NEXT var -> i
                               _ -> find_next var rest

-----------------------------------------------------------------------
--------------------- Evaluate Expression types -----------------------
-----------------------------------------------------------------------






eval_comp_expr arr e = (runReaderT $ eval_comp_expr' e) arr

eval_comp_expr' (Compare e1 e2 op') = do
  tab <- ask
  v1 <- liftIO $ (eval_expr tab e1)
  v2 <- liftIO $ (eval_expr tab e2)
  let op = case op' of
        "=" -> (==)
        "<>" -> (/=)
        ">" -> (>)
        ">=" -> (>=)
        "<" -> (<)
        "<=" -> (<=)
  return $ v1 `op` v2


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

         DivExpr e1 e2 -> do
           i1 <- liftIO $ r e1
           i2 <- liftIO $ r e2
           return $ i1 / i2

         SubExpr e1 e2 -> do
           i1 <- liftIO $ r e1
           i2 <- liftIO $ r e2
           return $ i1 - i2

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



print_expression :: Environment -> Expression -> IO ()
print_expression env e = do
  case e of
    e'@(StringColon _) -> putStr $ show e'
    e'@(StringComma _) -> putStr $ show e'
    e'@(String' _)     -> putStr $ show e'
    _ -> do
      e' <- liftIO $ eval_expr env e
      putStr $ show e'


interpreter   :: Int -> ReaderT Environment IO ()
interpreter n = do
  env@(Program tab program lines for_next next_for) <- ask
  let s = program ! n

  case s of
    (LET (Var i) e) -> do
      c <- liftIO $ (eval_expr env) e
      liftIO $ writeArray tab i (ConstExpr c)
      interpreter (n+1)


    (PRINT es) -> do
      liftIO $ sequence $ (print_expression env) <$> es
      liftIO $ putStrLn ""
      interpreter (n+1)

    END -> liftIO $ return ()

    INPUT (string) (Var c) -> do
      (liftIO . putStr) string
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

    GOSUB i -> do
      let l = case (lookup i lines) of
            Nothing -> l
            Just a -> a
      interpreter l
      interpreter (n+1)

    RETURN -> do liftIO $ return ()

    GOTO i -> do
      let l = case (lookup i lines) of
            Nothing -> l
            Just a -> a
      interpreter l

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
  let env = create_environment'' content symbol_table
  putStrLn $ show env
  (runReaderT (interpreter 1)) env

test_interp' file = do
  handle <- openFile file ReadMode
  content <- hGetContents handle
  symbol_table <-
     newArray ('A','Z') (ConstExpr 0) :: IO (IOArray Char Expression)
  let env = create_environment'' content symbol_table
  (runReaderT (interpreter 1)) env

{- 
   As of Sun 12/6/2020, using the test_interp or test_interp' function,
   the following program files execute just fine:

     foo.bas
     test.bas
     guess.bas
     fib.bas
     gcd.bas
     root.bas

   Still not quite working:

     sieve.bas
     pascal.bas
     bubblesort.bas
     amazing.bas
-}

test_parser file = do
  handle <- openFile file ReadMode
  content <- hGetContents handle
  let sorted_array = create_program_array content
  putStrLn $ show sorted_array


get_test_material file = do
  handle <- openFile file ReadMode
  content <- hGetContents handle
  table <- newArray ('A','Z') (ConstExpr 0) :: IO (IOArray Char Expression)
  let env = create_environment content table
  return (env)




test_io_array = newArray ('A','Z')
                         (ConstExpr 0) :: IO (IOArray Char Expression)

test_expr1 = (MultExpr (ConstExpr 2) (AddExpr (ConstExpr ( 3)) (ConstExpr ( 4.3))))
test_expr2 = (MultExpr ((Var 'A')) (AddExpr ((Var 'B')) ((Var 'C'))))
test_expr3 = (AddExpr ((Var 'A')) (AddExpr ((Var 'B')) ((Var 'C'))))

test_number_1   = ConstExpr ( 1)
test_number_5   = ConstExpr ( 5)
test_number_10  = ConstExpr ( 10)
test_var_x      = Var 'X'
test_var_y      = Var 'Y'
test_var_z      = Var 'Z'
test_valuevar   = VarVal test_var_x
test_valuefxn   = FxnVal (RND (test_var_x))
test_int_fxn_01 = INT (test_var_x)
test_int_fxn_02 = INT (AddExpr (test_var_x) (test_var_y))
test_int_fxn_03 = INT test_expr1
test_rnd_fxn_01 = RND (test_var_y)
test_rnd_fxn_02 = RND (AddExpr (test_var_x) (test_var_y))
test_rnd_fxn_03 = RND (test_expr1)

test_int_rnd_fxn_01
  = FxnExpr "INT" (FxnExpr "RND" test_number_5)
test_int_rnd_fxn_02
  = FxnExpr "INT" (MultExpr (FxnExpr "RND" test_number_5) (AddExpr ((Var 'H')) test_number_1))
test_int_rnd_fxn_03
  = FxnExpr "INT" (AddExpr (MultExpr (FxnExpr "RND" test_number_5) (Var 'H')) (test_number_1))

test_statement_for      = FOR test_var_x test_number_5 test_number_10
test_statement_forstep  = FORSTEP test_var_x test_number_5 test_number_10 test_number_1
test_statement_input    = INPUT "" test_var_y
test_statement_let      = LET test_var_x test_number_5
test_statement_next     = NEXT test_var_x
test_statement_nextlist = NEXTLIST [test_var_x, test_var_y, test_var_z]
multi_line_statement = "FOR T = 1 TO (N - R): PRINT \" \",: NEXT T"

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
test_program_print_1   = "PRINT \"Hello!\""
test_program_print_2   = "PRINT TAB(10); \"Hello!\""
test_pascal = "10 REM PASCAL'S TRIANGLE\n15 DIM V(100)\n20 INPUT \"NUMBER OF ROWS\"; N\n25 FOR T = 1 TO N: PRINT \" \",: NEXT T\n30 PRINT 1: PRINT\n35 LET V(1) = 1\n40 FOR R = 2 TO N\n45 PRINT: PRINT\n50 FOR T = 1 TO (N - R): PRINT \" \",: NEXT T\n55 PRINT \" \",\n60 FOR I = R TO 1 STEP -1\n65 LET V(I) = V(I) + V(I-1)\n70 PRINT V(I), \" \",\n75 NEXT I\n80 PRINT\n85 NEXT R\n90 END\n"
test_amazing = "10 PRINT TAB(28); \"AMAZING PROGRAM\"\n" ++
               "20 PRINT TAB(15);\"CREATIVE COMPUTING  MORRISTOWN, NEW JERSEY\"\n" ++
               "30 PRINT:PRINT:PRINT:PRINT\n" ++
               "100 INPUT \"WHAT ARE YOUR WIDTH AND LENGTH\";H,V\n"

showProgram f string = sequence $ putStrLn <$> (f string)


renumber l = case parse tuple_line l of
               [] -> [(0,newLine)]
               tup -> tup
               where newLine = (snd.head) (parse space l)



restructure s = concat $ (split ':') <$> (lines s)

tupled_lines' ls = [let l_statement = Unparsed_line (fst tuple) (snd tuple)
                            where tuple = head (renumber x)
                                    in l_statement | x <- restructure ls]
