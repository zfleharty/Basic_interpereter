module BasicTypes
  (Statement(..),
   Expression(..),
   CompareExpr(..),
   Constant(..),
   NegateExpr(..),
   PowerExpr(..),
   Value(..),
   Function(..),
   Var(..),
   Line_statement(..),
   Interpreter(..)) where

{-# LANGUAGE MultiParamTypeClasses #-}
data Statement      = FOR Var Expression Expression
                    | FORSTEP Var Expression Expression Expression
                    | IF CompareExpr Constant
                    | INPUT Var
                    | LET Var Expression
                    | NEXT Var
                    | PRINT Expression
                    | END

data Expression     = AddExpr Expression Expression
                    | MultExpr Expression Expression
                    | ConstExpr {const::Constant}
                    | Variable {var::Var}
                    | FxnExpr Function

data AndExpr        = AndExpr NotExpr AndExp
                    | AndNotExpr NotExpr

data NotExpr        = NotExpr CompareExpr
                    | NotNotExpr CompareExpr

data CompareExpr    = EqualsExpr Expression Expression
                    | CompNotEqualExpr AddExpr CompareExpr
                    | CompGreaterExpr AddExpr CompareExpr
                    | CompGreaterEqExpr AddExpr CompareExpr
                    | CompLessExpr AddExpr CompareExpr
                    | CompLessEqExpr AddExpr CompareExpr
                    | CompExpr AddExpr

data AddExpr        = AddExpr

data Var            = Var {character::Char}

data Constant       = NumConst {num::Int}
                    | StringConst {str::String}

data NegateExpr     = Neg PowerExpr
                    | Pexpr PowerExpr -- fix this!

data PowerExpr      = Pow Value PowerExpr
                    | PowValue Value

data Value          = ValueParens Expression
                    | ValueVar Var
                    | ValueFxn Function
                    | ValueConst Constant

data Function       = INT Expression
                    | RND Expression

data Line_statement = Unparsed_line {line_num:: Int, unparsed:: String}
                    | Parsed_line {ix:: Int, origLine:: Int, sttment:: Statement}

data Interpreter = Program {s_table :: [(Char,Constant)], program_counter:: Int}

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
  show (Variable x)     = show x
  show (FxnExpr x)      = show x

instance Show Var where
  show (Var x) = show (NoQuotesChar x)

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
  show (EqualsExpr a b) =  show a ++ " == " ++ show b

instance Show Constant where
  show (NumConst x) = show x
  show (StringConst x) = show (NoQuotes x)

instance Show Value where
  show (ValueVar x)   = show x
  show (ValueFxn x)   = show x
  show (ValueConst x) = show x
  show (ValueParens x) = show x

instance Show Function where
  show (INT e)    = "INT(" ++ (show e) ++ ")"
  show (RND e)    = "RND(" ++ (show e) ++ ")"

newtype NoQuotes = NoQuotes String
newtype NoQuotesChar = NoQuotesChar Char

instance Show NoQuotes where
  show (NoQuotes showstr) = showstr
instance Show NoQuotesChar where
  show (NoQuotesChar char) = show (NoQuotes [char])