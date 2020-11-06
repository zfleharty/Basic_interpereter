module BasicTypes
  (Statement(..),
   Expression(..),
   CompareExpr(..),
   Constant(..),
   NegateExp(..),
   PowerExp(..),
   Value(..),
   Function(..),
   Var(..),
   Line_statement(..),
   Interpereter(..)) where

{-# LANGUAGE MultiParamTypeClasses #-}
data Statement      = FOR Var Expression Expression
                    | FORSTEP Var Expression Expression Expression
                    | IF Expression Constant
                    | INPUT Var
                    | LET Var Expression
                    | NEXT Var
                    | PRINT Expression
                    | END 

-- class Interpreter a b where
--    eval :: a -> b

-- --class EvNumExpr a where
-- --   eval :: a -> Int

-- instance Interpreter Expression Int where
--   eval (AddExp e1 e2) = (eval e1) + (eval e2)
--   eval (MultExp e1 e2) = (eval e1) * (eval e2)
--   eval (ConstExp e) = num e

data Expression     = AddExp Expression Expression 
                    | MultExp Expression Expression
                    | ConstExp {cons::Constant}
                    | Variable {var::Var}



data CompareExpr    = EqualsExpr Expression Expression


data Var            = Var {character::Char}

data Constant       = NumConst {num::Int}
                    | StringConst {str::String}

data NegateExp      = Neg PowerExp
                    | Pexp PowerExp -- fix this!

data PowerExp      = Pow Value PowerExp
                    | PowValue Value

data Value          = ValueParens Expression
                    | ValueVar Var
                    | ValueFxn Function
                    | ValueConst Constant

data Function       = INT Expression
                    | RND Expression

data Line_statement = Unparsed_line {line_num:: Int, unparsed:: String} 
                    | Parsed_line {line_num:: Int, sttment:: Statement} 

data Interpereter = Program {s_table :: [(Char,Constant)], program_counter:: Int}

-------------------------------------------------------------
-- Derived instances for Data types                        --
-------------------------------------------------------------

instance Show Expression where
  show (AddExp e1 e2)  = (show e1) ++ " + " ++ (show e2)
  show (MultExp e1 e2) = (show e1) ++ " * " ++ (show e2)
--  show (EqualsExp e1 e2)  = (show e1) ++ " = " ++ (show e2)
  show (ConstExp x)       = show x
  show (Variable x)    = show x

instance Show Var where
  show (Var x) = show (NoQuotesChar x)

instance Eq Line_statement where
  a == b = (line_num a) == (line_num b)

instance Ord Line_statement where
  compare a b = compare (line_num a) (line_num b)

instance Show Line_statement where
  show x = "(" ++ show (line_num x) ++ ", " ++ unparsed x ++ ")" 

instance Show Statement where   
  show (FOR x e1 e2) =
    "FOR " ++ (show x) ++ " = " ++ (show e1) ++ " TO " ++ (show e2)
  show (FORSTEP x e1 e2 e3) =
    "FOR " ++ (show x) ++ " = " ++ (show e1)
    ++ " TO " ++ (show e2) ++ " STEP " ++ (show e3)
  show (IF e x)   = "IF " ++ (show e) ++ " THEN " ++ (show x)
  show (INPUT x)  = "INPUT " ++ (show x)
  show (LET x y)  = "LET " ++ (show x) ++ " = " ++ (show y)
  show (NEXT x)   = "NEXT " ++ (show x)
  show (PRINT e)  = "PRINT " ++ show e
  show (END)      = "END"                                   

instance Show Constant where
  show (NumConst x) = show x
  show (StringConst x) = show (NoQuotes x)

instance Show Value where
  show (ValueVar x)   = show x
  -- show (ValueFxn x)   = show x
  -- show (ValueConst x) = show x

instance Show Function where
  show (INT e)    = "INT(" ++ (show e) ++ ")"
  show (RND e)    = "RND(" ++ (show e) ++ ")"

newtype NoQuotes = NoQuotes String
newtype NoQuotesChar = NoQuotesChar Char

instance Show NoQuotes where show (NoQuotes str) = str
instance Show NoQuotesChar where show (NoQuotesChar char) = show (NoQuotes [char])
