module BasicTypes where


import System.Random
import Data.Array.IO
import Data.Array
import Data.Map hiding ((!),assocs)
import Prelude hiding (LT, GT)
{-# LANGUAGE MultiParamTypeClasses #-}
data Statement      = FOR Expression Expression Expression
                    | FORSTEP Expression Expression Expression Expression
                    | IF Expression Expression
                    | INPUT String Expression
                    | LET Expression Expression
                    | NEXT Expression
                    | NEXTLIST [Expression]
                    | GOTO Int
                    | PRINT Expression
                    | REM String
                    | END


data Expression     = AddExpr Expression Expression 
                    | MultExpr Expression Expression
                    | DivExpr Expression Expression
                    | ConstExpr {num::Float}
                    | ExpressionList [Expression]
                    | StringComma Expression 
                    | StringColon Expression 
                    | String' String
                    | Var {id:: Char}
                    | FxnExpr String Expression
                    | Compare Expression Expression (Float -> Float -> Bool)


data Expression' = Num' Int
                 | ID Char
                 | Op Expression' Expression' (Expression' -> Expression' -> Float)
                 | Comp Expression' Expression' (Expression' -> Expression' -> Bool)


data CompareExpr    = CompEqualsExpr Expression Expression
                    | LT Expression Expression
                    | GT Expression Expression
                    | LTE Expression Expression
                    | GTE Expression Expression
                    | NE Expression Expression


data NegateExpr     = Neg PowerExpr
                    | Pexpr PowerExpr -- fix this!

data PowerExpr      = Pow Value PowerExpr
                    | PowValue Value

data Value          = ParensVal Expression
                    | VarVal Expression
                    | FxnVal Function
                    | ConstVal Expression

data Function       = INT Expression
                    | RND Expression

data Line_statement = Unparsed_line {line_num:: Int, unparsed:: String}
                    | Parsed_line {ix       :: Int,
                                   origLine :: Int,
                                   sttment  :: Statement}

data Environment = Program {s_table        :: IOArray Char Expression,
                            basic_program  :: Array Int Statement,
                            line_map       :: Map Int Int,
                            for_next       :: Map Int Int,
                            next_for       :: Map Int Int}


-------------------------------------------------------------
-- Derived instances for Data types                        --
-------------------------------------------------------------

-- instance Num Expression where
--   (ConstExpr n1) + (ConstExpr n2) = (ConstExpr (n1 + n2))

instance Show CompareExpr where
  show (CompEqualsExpr e1 e2) = (show e1) ++ "==" ++ (show e2)
  show (LT e1 e2)             = (show e1) ++ "<" ++ (show e2)
  show (GT e1 e2)             = (show e1) ++ ">" ++ (show e2)
  show (LTE e1 e2)            = (show e1) ++ "<=" ++ (show e2)
  show (GTE e1 e2)            = (show e1) ++ ">=" ++ (show e2)
  show (NE e1 e2)             = (show e1) ++ "!=" ++ (show e2)

instance Show Environment where
  show (Program _ program mapping fNMap _ ) =
    "Environment{\n" ++
    "Symbol_table: " ++ "NO SHOW INSTANCE FOR IOARRAY YET\n" ++
    "Program:      " ++ show program ++ "\n" ++
    "line_map:     " ++ show mapping ++ "\n" ++
    "FOR->Next:    " ++ show fNMap ++ "}"

instance Show Expression where
  show (AddExpr e1 e2)
    = (show e1) ++ " + " ++ (show e2)

  show (MultExpr e1@(AddExpr e11 e12) e2@(AddExpr e21 e22))
    = "(" ++ (show e1) ++ ")" ++ " * " ++ "(" ++ (show e2) ++ ")"

  show (MultExpr e1@(AddExpr e11 e12) e2)
    = "(" ++ (show e1) ++ ")" ++ " * " ++ (show e2)

  show (MultExpr e1 e2@(AddExpr e21 e22)) =
    (show e1) ++ " * " ++ "(" ++ (show e2) ++ ")"

  show (MultExpr e1 e2) = (show e1) ++ " * " ++ (show e2)
  show (DivExpr e1 e2) = (show e1) ++ " / " ++ (show e2)
  show (ConstExpr x)    = show x
  show (Var x)          = show (NoQuotesChar x)
  show (StringColon e)  = show (e)
  show (StringComma e)  = show (e) ++ "\t"
  show (String' s)      = s
  show (FxnExpr s x)    = s ++ "(" ++ (show x) ++ ")"
  show (Compare e1 e2 _)= show e1 ++ " " ++ "?| " ++ show e2 

instance Eq Line_statement where
  a == b = (line_num a) == (line_num b)

instance Ord Line_statement where
  compare a b = compare (line_num a) (line_num b)

instance Show Line_statement where
  show (Unparsed_line n s)
    = "(" ++ show n ++ ", " ++ s ++ ")"
  show (Parsed_line i o stment)
    = "(" ++ show o ++ "->" ++ show i ++ ")," ++ show stment

instance Show Statement where
  show (FOR x e1 e2)
    = "FOR " ++ (show x) ++ " = " ++ (show e1) ++ " TO " ++ (show e2)
  show (FORSTEP x e1 e2 e3)
    = "FOR " ++ (show x) ++ " = " ++ (show e1) ++ " TO " ++ (show e2) ++
      " STEP " ++ (show e3)

  show (IF e x)  = "IF "    ++ (show e) ++ " THEN " ++ (show x)
  show (INPUT s x) = "INPUT " ++ s ++ " " ++ (show x)
  show (LET x y) = "LET "   ++ (show x) ++ " = "    ++ (show y)
  show (NEXT x)  = "NEXT "  ++ (show x)
  show (NEXTLIST (x:xs)) = "NEXT " ++ (show x) ++ (showCdr xs)
  show (PRINT e) = "PRINT " ++ (show e)
  show (REM s)   = "REM "   ++ (show (NoQuotes s))
  show (GOTO n) = "GOTO " ++ show n
  show (END)     = "END"

showCdr :: [Expression] -> String
showCdr [] = ""
showCdr (x:xs) = ", " ++ show x ++ (showCdr xs)

instance Show Value where
  show (VarVal x)    = show x
  show (FxnVal x)    = show x
  show (ConstVal x)  = show x
  show (ParensVal x) = show x

instance Show Function where
  show (INT e)    = "INT(" ++ (show e) ++ ")"
  show (RND e)    = "RND(" ++ (show e) ++ ")"

newtype NoQuotes = NoQuotes String
newtype NoQuotesChar = NoQuotesChar Char

instance Show NoQuotes where
  show (NoQuotes showstr) = showstr
  
instance Show NoQuotesChar where
  show (NoQuotesChar char) = show (NoQuotes [char])
