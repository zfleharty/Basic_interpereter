module BasicTypes where


import System.Random
import Data.Array.IO
import Data.Array
import Data.Map hiding ((!),assocs)

{-# LANGUAGE MultiParamTypeClasses #-}
data Statement      = FOR Expression Expression Expression
                    | FORSTEP Expression Expression Expression Expression
                    | IF CompareExpr Expression
                    | INPUT Expression
                    | LET Expression Expression
                    | NEXT Expression
                    | NEXTLIST [Expression]
                    | PRINT Expression
                    | REM String
                    | END

data Expression     = AddExpr Expression Expression 
                    | MultExpr Expression Expression
                    | ConstExpr {num::Float}
                    | Var {id:: Char}
                    | FxnExpr String Expression

data CompareExpr    = CompEqualsExpr Expression Expression

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
  show (ConstExpr x)    = show x
  show (Var x)          = show (NoQuotesChar x)
  -- show (FxnExpr s x)      = s ++ " " ++ show x
  show (FxnExpr s x)      = s ++ "(" ++ (show x) ++ ")"

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
  show (INPUT x) = "INPUT " ++ (show x)
  show (LET x y) = "LET "   ++ (show x) ++ " = "    ++ (show y)
  show (NEXT x)  = "NEXT "  ++ (show x)
  show (NEXTLIST (x:xs)) = "NEXT " ++ (show x) ++ (showCdr xs)
  show (PRINT e) = "PRINT " ++ (show e)
  show (REM s)   = "REM "   ++ (show (NoQuotes s))
  show (END)     = "END"

showCdr :: [Expression] -> String
showCdr [] = ""
showCdr (x:xs) = ", " ++ show x ++ (showCdr xs)

instance Show CompareExpr where
  show (CompEqualsExpr a b) =  show a ++ " = " ++ show b

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
