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
import Data.Map hiding ((!),assocs,split,take,drop)
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


-------------------------------------------------------------------------------------------
------------ Helper functions used to split up lines before parsing -----------------------
-------------------------------------------------------------------------------------------
tuple_line  :: Parser Int
split :: Parser [[Char]]
renumber    :: String -> [(Int, String)]
restructure :: String -> [[Char]]

split = dont_split_colon +++ split_colon

split_colon = do
  {
    s0 <- token $ todelim ':';
    token $ char ':';
    s1 <- token split_colon;
    return $ [s0]++s1
  } +++ do {s <- todelim ':'; return [s]}

dont_split_colon = do
  {
    s0 <- token $ todelim_include '\"';
    s1 <- token $ todelim_include ':';
    s2 <- token $ todelim_include '\"';
    return $ [s0 ++ s1 ++ s2]
  }


renumber l = case parse tuple_line l of
               [] -> [(0,newLine)]
               tup -> tup
               where newLine = (snd.head) (parse space l)

restructure s = concat $ ((fst.head) . (parse split)) <$> (lines s)


tuple_line    = do {num <- int; return num}


-----------------------------------------------------------------------------------
------------------ Functions used to create environment ----------------------------
-----------------------------------------------------------------------------------
--create_environment :: [Char] -> IOArray Char Expression -> Environment
tupled_lines       :: String -> [Line_statement]
parse_lines        :: (Num a, Enum a) => String -> [((Int, a), Statement)]
--find_next          :: Num p => t -> [(p, Statement)] -> p

create_environment content table ar_table = Program table ar_table program (fromList lm) (fromList f_n) (fromList $ fmap swap f_n)
  where (lm,sa) = unzip $ parse_lines (content)
        program = array (1,length sa) [x | x <- zip [1..] sa]
        list_prog = assocs program
        f_n = [((id' v,i),find_next v (drop i list_prog)) | (i,(FOR v _ _ _)) <- list_prog]
        
tupled_lines ls   = [let l_statement = Unparsed_line (fst tuple) (snd tuple)
                            where tuple = head (renumber x)
                                    in l_statement | x <- restructure ls]

parse_lines lines = [let p_line = (lmap,((fst . head) stment))
                            where stment   = parse statement (unparsed ls)
                                  lmap = (line_num ls,n)
                      in p_line | (n,ls) <- zip [1..] ((tupled_lines lines))]



find_next _ []             = ('?',0)
find_next var' ((i,s):rest) = case s of
                               NEXT (IDList ids) -> case (elem var' ids) of
                                 True -> (id' var',i)
                                 False -> find_next var' rest
                               NEXT (single_var) -> case (single_var == var') of
                                 True -> (id' var', i)
                                 False -> find_next var' rest
                               _ -> find_next var' rest

-----------------------------------------------------------------------
--------------------- Evaluate Expression types -----------------------
-----------------------------------------------------------------------

eval_comp_expr arr e = (runReaderT $ eval_comp_expr' e) arr

eval_comp_expr' e = do
  tab <- ask
  case e of
    (NotExpr e') -> do
      tf <- eval_comp_expr' e'
      return $ not tf
    (AndExpr e1 e2) -> do
      v1 <- eval_comp_expr' e1
      v2 <- eval_comp_expr' e2
      return $ v1 && v2
    (OrExpr e1 e2) -> do
      v1 <- eval_comp_expr' e1
      v2 <- eval_comp_expr' e2
      return $ v1 || v2
    (Compare e1 e2 op') -> do
      v1 <- liftIO $ (eval_expr tab e1)
      v2 <- liftIO $ (eval_expr tab e2)
--      liftIO $ putStrLn $ "Could not match case:" ++ op'
      let op = case op' of
            "=" -> (==)
            "<>" -> (/=)
            ">" -> (>)
            ">=" -> (>=)
            "<" -> (<)
            "<=" -> (<=)
      return $ v1 `op` v2


eval_expr env e = (runReaderT $ eval_expr' e) env

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
             
         Array c e' -> do
           i <- liftIO $ r e'           
           (OneDArray arr) <- liftIO $ readArray (array_table env) c
           (ConstExpr value) <- liftIO $ readArray (arr) (fromIntegral.floor $ i)
           return value

         (Var v) -> do
           constValue <- liftIO $ (readArray tab v)
           return $ (num constValue)

  

-- experimenting here by Hoss
print_expression :: Environment -> Expression -> IO ()
print_expression env e = do
  case e of
    (StringColon e') -> do
      case e' of
        String' e'' -> putStr $ show e'
        _      -> do
          e'' <- liftIO $ eval_expr env e'
          putStr $ show e''
    (StringComma e') -> do
      case e' of
        String' e'' -> putStr $ show e' ++ "\t"
        _      -> do
          e'' <- liftIO $ eval_expr env e'
          putStr $ show e'' ++ "\t"
    (String' e')     -> putStrLn $ show e'
    _ -> do
      e' <- liftIO $ eval_expr env e
      putStrLn $ show e'



toInt = (fromIntegral.floor)

for_next_check env (FOR var' _ finish step) = do
  finish' <- liftIO $ (eval_expr env) finish
  (ConstExpr value) <- liftIO $ readArray (s_table env) (id' var')
  s' <- liftIO $ (eval_expr env) step
  let conditional = (case s' < 0 of
                       True -> (>)
                       False -> (<))
  return $ ((value `conditional` finish'),s')


interpreter   :: Int -> ReaderT Environment IO ()
interpreter n = do
  env@(Program tab ar_table program lines for_next next_for) <- ask
  let s = program ! n

  case s of
    (LET (Var i) e) -> do
      c <- liftIO $ (eval_expr env) e
      liftIO $ writeArray tab i (ConstExpr c)
      interpreter (n+1)

    (LET (Array c i) e) -> do
      value <- liftIO $(eval_expr env) e
      i' <- liftIO $  (eval_expr env) i
      (OneDArray arr) <- liftIO $ readArray ar_table c
      liftIO $ writeArray arr (toInt i') (ConstExpr value)
      interpreter (n+1)
    
    DIM (Array c e) -> do                                                             
      size <- liftIO $ eval_expr env e                         
      arr <- liftIO $ (newArray (0,(fromIntegral.floor) size) (ConstExpr 0) :: IO (IOArray Int Expression))
      liftIO $ writeArray ar_table c (OneDArray arr)                                  
      interpreter (n+1)                                                               
    
      
    (PRINT es) -> do
      case es of
        []  -> liftIO $ sequence [putStrLn ""]
        es' -> liftIO $ sequence $ (print_expression env) <$> es'
      interpreter (n+1)

    END -> liftIO $ return ()

    INPUT (string) (Var c) -> do
      (liftIO . putStr) string
      inp <- liftIO $ readLn
      liftIO $ writeArray tab c (ConstExpr inp)
      interpreter (n + 1)

    INPUT (string) (IDList ids) -> do
      (liftIO . putStr) string
      inputs <- liftIO $ (sequence $ take (length ids) (repeat $ readLn))

      let assign = (\(c,inp) -> writeArray tab c (ConstExpr inp))
      liftIO $ traverse assign $ zip (id' <$> ids) inputs
      interpreter (n+1)

    -- a temp klunky way to accommodate a 2-input statement
    -- ugly, I know
    INPUTMULTI (string) [Var c1, Var c2] -> do
      (liftIO . putStr) string
      inp1 <- liftIO $ readLn
      inp2 <- liftIO $ readLn
      liftIO $ writeArray tab c1 (ConstExpr inp1)
      liftIO $ writeArray tab c2 (ConstExpr inp2)
      interpreter (n+1)


    FOR (Var c) e1 e2 step -> do
      start <- liftIO $ (eval_expr env) e1
      finish <- liftIO $ (eval_expr env) e2
      liftIO $ writeArray tab c (ConstExpr start)
      s' <- liftIO $ (eval_expr env) step
      let conditional = (case s' < 0 of
                           True -> (<)
                           False -> (>))

      if start `conditional` finish
        then case (lookup (c,n) for_next) of
               Nothing -> do
                 liftIO $ putStrLn "NextNotFound"
                 interpreter (n + 1)
               Just (_,l) -> do
                 interpreter (l + 1)
        else interpreter (n + 1)




    NEXT (Var c) -> do
      let for_line = case (lookup (c,n) next_for) of
            Nothing -> n
            Just (_,l) -> l
      (bool,s'') <- for_next_check env (program ! for_line)
      (ConstExpr value) <- liftIO $ readArray tab c
                        
      if bool
        then do
        liftIO $ writeArray tab c (ConstExpr (value + s''))
        interpreter (for_line + 1)
        else interpreter (n + 1)




        

    NEXT (IDList ids) -> do
      --NEXT J, I
      let keys = (,) <$> (id' <$> ids) <*> pure n -- [('J',20),('I',20)]
      let values = (`lookup` next_for) <$> keys --[Just ('J',15),Just ('I',14)]
      let get_line = (\value -> case value of
                                  Nothing -> n
                                  Just (_,l) -> l)
      let for_lines = [ l | l <- (program !) <$> get_line <$> values]
      
      let finish_expressions = [ e2 | (FOR _ _ e2 st) <- (program !) <$> get_line <$> values] --[N - I,N - 1.0]
      evaled_expressions <- liftIO $ traverse (eval_expr env) finish_expressions --[9.0,9.0]
      evaled_values <- liftIO $ traverse (eval_expr env) ids -- [1.0,1.0]
      
--      let continue_list <- traverse $ (for_next_check env) <$> for_lines
      let next_interpret = [ (i,l,v) | ((Just (i,l)),f,v) <- zip3 values evaled_expressions evaled_values, v < f]

      case next_interpret of
        [] -> interpreter (n + 1)
        ((i,l,v):_) -> do
          liftIO $ writeArray tab i (ConstExpr (v + 1))
          interpreter (l + 1)







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
    REM _ -> interpreter (n+1)
      
    a -> do
      liftIO $ putStrLn $ "could not match" ++ show a
      interpreter (n+1)


main = do
  --args <- getArgs
  --fileExists <- doesFileExist $ head args
  if True -------------------- fileExists __________________Changed for testing inside interactive GHCI without command line args
    then do handle <- openFile ("test.bas") ReadMode
            content <- hGetContents handle

            table <- newArray ('A','Z')
                     (ConstExpr 0) :: IO (IOArray Char Expression)

            putStrLn "FINISH LINE"
            -- putStrLn $ show f_n
            -- putStrLn $ show n_f
            -- putStrLn $ show linemap
            -- putStrLn $ show sorted_ar
    else do putStrLn $ "File " ++ ("") ++ " Does Not Exist."



-- ================================================================= --
--          Some convenience definitions for testing                 --
-- ================================================================= --


test_interp file = do
  handle <- openFile file ReadMode
  content <- hGetContents handle
  symbol_table <- newArray ('A','Z') (ConstExpr 0) :: IO (IOArray Char Expression)
  ar_table <- newArray ('A','Z') (ConstExpr 0) :: IO (IOArray Char Expression)
  let env = create_environment content symbol_table ar_table
  putStrLn $ show env
  (runReaderT (interpreter 1)) env

test_interp' file = do
  handle <- openFile file ReadMode
  content <- hGetContents handle
  symbol_table <- newArray ('A','Z') (ConstExpr 0) :: IO (IOArray Char Expression)
  ar_table <- newArray ('A','Z') (ConstExpr 0) :: IO (IOArray Char Expression)
  let env = create_environment content symbol_table ar_table
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

     sieve.bas:
         (1) Need a single 1-D array to hold integer values
     pascal.bas:
         (1) Need a single 1-D array to hold integer values
     bubblesort.bas:
         (1) Need a single 1-D array to hold integer values
     amazing.bas:
         (1) Need a dual-input INPUT statement, suggesting we
             generalize the INPUT statement from
             INPUT String Expression to INPUT String [Expression]
             or even INPUT String [Var]
         (2) Need multiple 2-D arrays (the arrays would hold integers)

-}

test_parser file = do
  handle <- openFile file ReadMode
  content <- hGetContents handle
  table <- newArray ('A','Z') (ConstExpr 0) :: IO (IOArray Char Expression)
  ar_table <- newArray ('A','Z') (ConstExpr 0) :: IO (IOArray Char Expression)
  let env = create_environment content table ar_table
  let sorted_array = basic_program env
  sequence $ (putStrLn.show) <$> sorted_array


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
test_program_if_1   = "IF X<>1 AND Y<>1 THEN 100"
test_program_if_2   = "IF A(J) <= A(J+1) THEN 2070"
test_program_print_1   = "PRINT \"Hello!\""
test_program_print_2   = "PRINT TAB(10); \"Hello!\""
test_program_input_2   = "INPUT \"Enter Inputs\"; X, Y"
test_pascal = "10 REM PASCAL'S TRIANGLE\n15 DIM V(100)\n20 INPUT \"NUMBER OF ROWS\"; N\n25 FOR T = 1 TO N: PRINT \" \",: NEXT T\n30 PRINT 1: PRINT\n35 LET V(1) = 1\n40 FOR R = 2 TO N\n45 PRINT: PRINT\n50 FOR T = 1 TO (N - R): PRINT \" \",: NEXT T\n55 PRINT \" \",\n60 FOR I = R TO 1 STEP -1\n65 LET V(I) = V(I) + V(I-1)\n70 PRINT V(I), \" \",\n75 NEXT I\n80 PRINT\n85 NEXT R\n90 END\n"
test_amazing = "10 PRINT TAB(28); \"AMAZING PROGRAM\"\n" ++
               "20 PRINT TAB(15);\"CREATIVE COMPUTING  MORRISTOWN, NEW JERSEY\"\n" ++
               "30 PRINT:PRINT:PRINT:PRINT\n" ++
               "100 INPUT \"WHAT ARE YOUR WIDTH AND LENGTH\";H,V\n" ++
               "102 IF H<>1 AND V<>1 THEN 110\n" ++
               "104 PRINT \"MEANINGLESS DIMENSIONS.  TRY AGAIN.\":GOTO 100\n" ++
               "110 DIM W(H,V),V(H,V)\n" ++
               "120 PRINT\n" ++
               "130 PRINT\n" ++
               "140 PRINT\n" ++
               "150 PRINT\n" ++
               "160 Q=0:Z=0:X=INT(RND(1)*H+1)\n" ++
               "180 PRINT \":  \";\n"
showProgram f string = sequence $ putStrLn <$> (f string)
