----------------------------------------------------------------------}
{-                                                                    -}
{- CS 556: Adv Declarative Programming                                -}
{- Fall 2020                                                          -}
{- Project 03: BASIC parser                                           -}
{- Zach Fleharty & Hoss Craft                                         -}
{- submitted …                                                        -}
{-                                                                    -}
{----------------------------------------------------------------------}
{- A Haskell-based implementation of a parser and interpreter for
   the BASIC language. 
                                                                      -}
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


------------------------------------------------------------------------
--------- Helper functions used to split up lines before parsing -------
------------------------------------------------------------------------

split :: Parser [[Char]]
split_colon :: Parser [[Char]]
dont_split_colon :: Parser [[Char]]
renumber    :: String -> [(Int, String)]
restructure :: String -> [[Char]]
tuple_line  :: Parser Int

split = dont_split_colon +++ split_colon

split_colon = do
  {
    s0 <- todelim ':';
    token $ char ':';
    s1 <- split_colon;
    return $ [s0]++s1
  } +++ do {s <- todelim ':'; return [s]}

dont_split_colon = do
  {
    s0 <- todelim_include '\"';
    s1 <- todelim_include ':';
    s2 <- todelim_include '\"';
    s3 <- many item;
    return $ [s0 ++ s1 ++ s2 ++ s3]
  }

renumber l = case parse tuple_line l of
               [] -> [(0,newLine)]
               tup -> tup
               where newLine = (snd.head) (parse space l)

restructure s = concat $ ((fst.head) . (parse split)) <$> (lines s)

tuple_line    = do {num <- int; return num}


------------------------------------------------------------------------
--------- Functions used to create environment -------------------------
------------------------------------------------------------------------

create_environment :: String -> IOArray Char Expression
                      -> IOArray Char Expression -> Environment
tupled_lines       :: String -> [Line_statement]
parse_lines        :: (Num a, Enum a) => String -> [((Int, a), Statement)]
find_next          :: Num p => Expression -> [(p, Statement)] -> (Char, p)

create_environment content table ar_table =
  Program table ar_table program (fromList lm) (fromList f_n) (fromList $ fmap swap f_n)
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

------------------------------------------------------------------------
--------- Evaluate Expression types ------------------------------------
------------------------------------------------------------------------

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
           case e' of
             (ExpressionList es) -> do
               (a:b:[]) <- liftIO $ sequence $ r <$> es
               (TwoDArray arr) <- liftIO $ readArray (array_table env) c
               (ConstExpr value) <- liftIO $ readArray arr (toInt a,toInt b)
               return value
             _ -> do
               i <- liftIO $ r e'           
               (OneDArray arr) <- liftIO $ readArray (array_table env) c
               (ConstExpr value) <- liftIO $ readArray (arr) (toInt i)
               return value

         (Var v) -> do
           constValue <- liftIO $ (readArray tab v)
           return $ (num constValue)

  
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
                       True -> (>=)
                       False -> (<=))
  return $ (((value + s') `conditional` finish'),s')


create_array env e = do
  case e of
    (ExpressionList es)-> do
      (w:h:[]) <- liftIO $ sequence $ (eval_expr env) <$> es
      arr <- liftIO $ newArray ((0,0),(toInt w,toInt h)) (ConstExpr 0) :: IO (IOArray (Int,Int) Expression)
      return $ TwoDArray arr
    _ -> do
      l <- liftIO $  (eval_expr env) e
      arr <- liftIO $ newArray (0,toInt l) (ConstExpr 0) :: IO (IOArray Int Expression)
      return $ OneDArray arr

      
    

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
      case i of
        (ExpressionList es) -> do
          (w:h:[]) <- liftIO $ sequence $ (eval_expr env) <$> es
          (TwoDArray arr) <- liftIO $ readArray ar_table c
          liftIO $ writeArray arr (toInt w, toInt h) (ConstExpr value)
        _ -> do
          i' <- liftIO $  (eval_expr env) i
          (OneDArray arr) <- liftIO $ readArray ar_table c
          liftIO $ writeArray arr (toInt i') (ConstExpr value)
      interpreter (n+1)
    
    DIM (Array c e) -> do                                                             
      arr <- liftIO $ create_array env e
      liftIO $ writeArray ar_table c arr                                  
      interpreter (n+1)                                                               
    
    DIM (ArrayList arrs) -> do
      let eval' = (\(f,ls) -> liftIO $ sequence $ f <$> ls)
      let dimensions = (size' <$> arrs)
      let is = i <$> arrs
      arrs' <- eval' (create_array env, dimensions)
      let write_array = (\(i',a) -> writeArray ar_table i' a)

      eval' $ (write_array,(zip is arrs'))
      interpreter (n+1)


    (PRINT es) -> do
      case es of
        []  -> liftIO $ sequence [putStrLn ""]
        es' -> liftIO $ sequence $ (print_expression env) <$> es'
      interpreter (n+1)

    

    (ON e is) -> do
      e' <- liftIO $ (eval_expr env) e
      if (toInt e') > (length is) then interpreter (n + 1)
        else case (lookup ((last.take (toInt e')) is) lines) of
               Nothing -> interpreter (n+1)
               (Just l) -> interpreter l
    

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

      let finish_expressions = [ e2 | (FOR _ _ e2 _) <-
                                   (program !) <$> get_line <$> values]

      evaled_expressions <- liftIO $ traverse (eval_expr env) finish_expressions
      evaled_values <- liftIO $ traverse (eval_expr env) ids 
      

      let next_interpret = [ (i,l,v) | ((Just (i,l)),f,v) <-
                               zip3 values evaled_expressions evaled_values, v < f]

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

    END -> liftIO $ return ()

    ASSIGNMENT (Array i d) e -> do
      e' <- liftIO $ (eval_expr env) e
      case d of
        (ExpressionList es) -> do
          (w:h:[]) <- liftIO $ traverse (eval_expr env)  es
          (TwoDArray arr) <- liftIO $ readArray (ar_table) i
          liftIO $ writeArray arr (toInt w, toInt h) (ConstExpr e')
        _ -> do
          ix' <- liftIO $ (eval_expr env) d
          (OneDArray arr) <- liftIO $ readArray (ar_table) i
          liftIO $ writeArray arr (toInt ix') (ConstExpr e')
      interpreter (n+1)
          


    ASSIGNMENT (Var c) e2 -> do
      e <- liftIO $ (eval_expr env) e2
      liftIO $ writeArray tab c (ConstExpr e)
      interpreter (n+1)



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
--          and performing parse/interpretations in ghci             --
-- ================================================================= --

-- ---------------------------------------------------------------------
-- test_interp     Processes, parses, and interprets a BASIC code file.
--                 Usage, from interactive gchi:
--                     test_interp "foo.bas"
-- ---------------------------------------------------------------------
test_interp :: FilePath -> IO ()
test_interp file = do
  handle <- openFile file ReadMode
  content <- hGetContents handle
  symbol_table <- newArray ('A','Z') (ConstExpr 0) :: IO (IOArray Char Expression)
  ar_table <- newArray ('A','Z') (ConstExpr 0) :: IO (IOArray Char Expression)
  let env = create_environment content symbol_table ar_table
  (runReaderT (interpreter 1)) env

-- ---------------------------------------------------------------------
-- test_interp'    Same as test_interp, but also prints out the
--                 environment information (useful for debugging)
-- ---------------------------------------------------------------------
test_interp' :: FilePath -> IO ()
test_interp' file = do
  handle <- openFile file ReadMode
  content <- hGetContents handle
  symbol_table <- newArray ('A','Z') (ConstExpr 0) :: IO (IOArray Char Expression)
  ar_table <- newArray ('A','Z') (ConstExpr 0) :: IO (IOArray Char Expression)
  let env = create_environment content symbol_table ar_table
  putStrLn $ show env
  (runReaderT (interpreter 1)) env

-- ---------------------------------------------------------------------
-- test_parser     Processes and parses (but does not interpret) a
--                 BASIC code file. Usage, from interactive gchi:
--                     test_parser "foo.bas"
-- ---------------------------------------------------------------------
test_parser :: FilePath -> IO (Array Int ())
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

test_foo     = "10 LET A = 2\n20 LET B = 3\n30 LET C = 4\n" ++
               "40 PRINT A * (B + C)\n50 END"

test_amazing =
  "10 PRINT TAB(28); \"AMAZING PROGRAM\"\n" ++
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
