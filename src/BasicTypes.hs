{----------------------------------------------------------------------}
{-                                                                    -}
{- CS 556: Adv Declarative Programming                                -}
{- Fall 2020                                                          -}
{- Project 03: BASIC parser                                           -}
{- Zach Fleharty & Hoss Craft                                         -}
{- Submitted Sat 12/12/2020                                           -}
{-                                                                    -}
{----------------------------------------------------------------------}
{- BasicTypes.hs
   Required for Interpreter.hs, which is the main file for a
   Haskell-based implementation of a parser/interpreter for the
   BASIC language. Also see related required files Parser.hs, and
   Parselib.hs.
                                                                      -}
{----------------------------------------------------------------------}

module BasicTypes where


import System.Random
import Data.Array.IO
import Data.Array
import Data.Map hiding ((!),assocs)
import Prelude hiding (LT, GT)
{-# LANGUAGE MultiParamTypeClasses #-}
data Statement      = FOR Expression Expression Expression Expression
                    | DIM Expression
                    | IF Expression Expression
                    | INPUT String Expression
                    | LET Expression Expression
                    | NEXT Expression
                    | GOTO Int
                    | GOSUB Int
                    | PRINT [Expression]
                    | REM String
                    | END
                    | RETURN
                    | ON Expression [Int]
                    | ASSIGNMENT Expression Expression

data Expression     = AddExpr Expression Expression
                    | SubExpr Expression Expression
                    | MultExpr Expression Expression
                    | DivExpr Expression Expression
                    | ConstExpr {num::Float}
                    | ExpressionList [Expression]
                    | StringComma Expression 
                    | StringColon Expression 
                    | String' String
                    | Var {id'::Char}
                    | IDList {exp_list::[Expression]}
                    | ArrayList [Expression]
                    | Array {i::Char, size'::Expression}
                    | FxnExpr String Expression
                    | Compare Expression Expression String
                    | NotExpr Expression
                    | AndExpr Expression Expression
                    | OrExpr Expression Expression
                    | OneDArray (IOArray Int Expression)
                    | TwoDArray (IOArray (Int,Int) Expression)

data Function       = INT Expression
                    | RND Expression

data Line_statement = Unparsed_line {line_num:: Int, unparsed:: String}
                    | Parsed_line {ix       :: Int,
                                   origLine :: Int,
                                   sttment  :: Statement}

data Environment = Program {s_table        :: IOArray Char Expression,
                            array_table    :: IOArray Char Expression,
                            basic_program  :: Array Int Statement,
                            line_map       :: Map Int Int,
                            for_next       :: Map (Char,Int) (Char,Int),
                            next_for       :: Map (Char,Int) (Char,Int)}


-------------------------------------------------------------
-- Derived instances for Data types                        --
-------------------------------------------------------------

instance Eq Expression where
  (Var x) == (Var y) = x == y
  
instance Show Environment where
  show (Program _ _ program mapping fNMap nFMap ) =
    "Environment{\n" ++
    "Symbol_table: " ++ "NO SHOW INSTANCE FOR IOARRAY YET\n" ++
    "array_table: " ++ "NO SHOW INSTANCE FOR IOARRAY YET\n" ++
    "Program:      " ++ show program ++ "\n" ++
    "line_map:     " ++ show mapping ++ "\n" ++
    "FOR->Next:    " ++ show fNMap ++ 
    "Next->For:    " ++ show nFMap ++ "}"

instance Show Expression where

  show (MultExpr e1@(AddExpr e11 e12) e2@(AddExpr e21 e22))
    = "(" ++ (show e1) ++ ")" ++ " * " ++ "(" ++ (show e2) ++ ")"

  show (MultExpr e1@(AddExpr e11 e12) e2)
    = "(" ++ (show e1) ++ ")" ++ " * " ++ (show e2)

  show (MultExpr e1 e2@(AddExpr e21 e22))
    = (show e1) ++ " * " ++ "(" ++ (show e2) ++ ")"
  show (AddExpr e1 e2)    = (show e1) ++ " + " ++ (show e2)
  show (SubExpr e1 e2)    = (show e1) ++ " - " ++ (show e2)
  show (MultExpr e1 e2)   = (show e1) ++ " * " ++ (show e2)
  show (DivExpr e1 e2)    = (show e1) ++ " / " ++ (show e2)
  show (ConstExpr x)      = show x
  show (Var x)            = show (NoQuotesChar x)
  show (StringColon e)    = show (e) ++ "col"
  show (StringComma e)    = show (e) ++ "\t"
  show (String' s)        = s 
  show (FxnExpr s x)      = s ++ "(" ++ (show x) ++ ")"
  show (Compare e1 e2 op) = show e1 ++ " " ++ show op ++ " " ++ show e2
  show (NotExpr e)        = "NOT " ++ show e
  show (AndExpr e1 e2)    = show e1 ++ " AND " ++ show e2
  show (OrExpr e1 e2)     = show e1 ++ "OR" ++ show e2
  show (IDList es)        = "IDList" ++ show es
  show (Array c e)    = show (NoQuotesChar c) ++ "(" ++ show e ++ ")"
  show (ArrayList es)     = show es
  show (ExpressionList es)= show es
  
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
  show (FOR x e1 e2 e3)
    = "FOR " ++ (show x) ++ " = " ++ (show e1) ++ " TO " ++
                (show e2) ++ " {" ++ show e3++ "}"
  show (IF e x)          = "IF "    ++ (show e) ++ " THEN " ++ (show x)
  show (DIM e)           = "DIM " ++ show e
  show (INPUT s x)       = "INPUT " ++ s ++ " " ++ (show x)
  show (LET x y)         = "LET "   ++ (show x) ++ " = "    ++ (show y)
  show (NEXT x)          = "NEXT "  ++ (show x)
  show (PRINT e)         = "PRINT " ++ (show e)
  show (REM s)           = "REM "   ++ (show (NoQuotes s))
  show (GOTO n)          = "GOTO " ++ show n
  show (GOSUB n)         = "GOSUB" ++ show n
  show (END)             = "END"
  show (RETURN)          = "RETURN"
  show (ASSIGNMENT c e)  = show c ++ " = " ++ show e
  show (ON e1 e2)        = "ON " ++ show e1 ++ " GOTO " ++ show e2
showCdr :: [Expression] -> String
showCdr [] = ""
showCdr (x:xs) = ", " ++ show x ++ (showCdr xs)

instance Show Function where
  show (INT e)    = "INT(" ++ (show e) ++ ")"
  show (RND e)    = "RND(" ++ (show e) ++ ")"

newtype NoQuotes     = NoQuotes String
newtype NoQuotesChar = NoQuotesChar Char

instance Show NoQuotes where
  show (NoQuotes showstr) = showstr
  
instance Show NoQuotesChar where
  show (NoQuotesChar char) = show (NoQuotes [char])
