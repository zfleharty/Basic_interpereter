module BasicTypes where


import System.Random

{-# LANGUAGE MultiParamTypeClasses #-}
data Statement      = FOR Expression Expression Expression
                    | FORSTEP Expression Expression Expression Expression
                    | IF CompareExpr Expression
                    | INPUT Expression
                    | LET Expression Expression
                    | NEXT Expression
                    | PRINT Expression
                    | END

data Expression     = AddExpr Expression Expression
                    | MultExpr Expression Expression
                    | ConstExpr {num::Float}
                    | Var {id:: Char}
                    | FxnExpr Function

--                    | CompExpr Expression Expression (Expression -> Expression -> Bool)

data CompareExpr    = CompEqualsExpr Expression Expression

--data Var            = Var {character::Char}

--data Constant       = NumConst {num::Float}
--                    | StringConst {str::String}

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
                    | Parsed_line {ix:: Int, origLine:: Int, sttment:: Statement}

data Program = ProgInfo {gen:: StdGen}

data Interpreter = Program {s_table :: [(Char,Expression)], program_counter:: Int}

-------------------------------------------------------------
-- Derived instances for Data types                        --
-------------------------------------------------------------

instance Show Expression where
  show (AddExpr e1 e2)  = (show e1) ++ " + " ++ (show e2)
  show (MultExpr e1@(AddExpr e11 e12) e2@(AddExpr e21 e22)) =
    "(" ++ (show e1) ++ ")" ++ " * " ++ "(" ++ (show e2) ++ ")"
  show (MultExpr e1@(AddExpr e11 e12) e2) = 
    "(" ++ (show e1) ++ ")" ++ " * " ++ (show e2)
  show (MultExpr e1 e2@(AddExpr e21 e22)) =
    (show e1) ++ " * " ++ "(" ++ (show e2) ++ ")"
  show (MultExpr e1 e2) = (show e1) ++ " * " ++ (show e2)
  show (ConstExpr x)    = show x
  show (Var x)     = show x
  show (FxnExpr x)      = show x

instance Eq Line_statement where
  a == b = (line_num a) == (line_num b)

instance Ord Line_statement where
  compare a b = compare (line_num a) (line_num b)

instance Show Line_statement where
  show (Unparsed_line n s) = "(" ++ show n ++ ", " ++ s ++ ")"
  show (Parsed_line i o stment) = "(" ++ show o ++ "->" ++ show i
                                  ++ ")," ++ show stment

instance Show Statement where
  show (FOR x e1 e2) =
    "FOR " ++ (show x) ++ " = " ++ (show e1) ++ " TO " ++ (show e2)
  show (FORSTEP x e1 e2 e3) =
    "FOR " ++ (show x) ++ " = " ++ (show e1)
    ++ " TO " ++ (show e2) ++ " STEP " ++ (show e3)
  show (IF e x)  = "IF "    ++ (show e) ++ " THEN " ++ (show x)
  show (INPUT x) = "INPUT " ++ (show x)
  show (LET x y) = "LET "   ++ (show x) ++ " = "    ++ (show y)
  show (NEXT x)  = "NEXT "  ++ (show x)
  show (PRINT e) = "PRINT " ++ (show e)
  show (END)     = "END"

instance Show CompareExpr where
  show (CompEqualsExpr a b) =  show a ++ " = " ++ show b

-- instance Show Constant where
--   show (NumConst x) = show x
--   show (StringConst x) = show (NoQuotes x)

instance Show Value where
  show (VarVal x)   = show x
  show (FxnVal x)   = show x
  show (ConstVal x) = show x
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
