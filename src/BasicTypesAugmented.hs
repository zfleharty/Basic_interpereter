module BasicTypesAugmented
  (Statement(..),
   IDList(..),
   ArrayList(..),
   IntegerList(..),
   ExpressionList(..),
   PrintList(..),
   Expression(..),
   AndExpr(..),
   NotExpr(..),
   CompareExpr(..),
   AddExpr(..),
   MultExpr(..),
   NegateExpr(..),
   PowerExpr(..),
   Value(..),
   Var(..),
   Array(..),
   Function(..),
   Constant(..),
   ID(..),
   Line_statement(..),
   Interpreter(..)) where

{-# LANGUAGE MultiParamTypeClasses #-}

data Statement      = DIM ArrayList
                    | END
                    | FOR ID Expression Expression
                    | FORSTEP Var Expression Expression Expression
                    | GOTO Int
                    | GOSUB Int
                    | IF Expression Int
                    | INPUT String IDList
                    | LET Var Expression
                    | NEXT IDList
                    | ON Expression IntegerList
                    | PRINT PrintList
                    | PRINT_TAB Value PrintList
                    | REM String
                    | RETURN
                    | EQUALS Var Expression

data IDList         = IDList [ID]

data ArrayList      = ArrayList [Array]

data IntegerList    = IntegerList [Int]

data ExpressionList = ExpressionList [Expression]

data PrintList      = PrintList Expression
                    | PrintListComma [Expression]      -- tab sep
                    | PrintListSemiColon [Expression]  -- space sep 

data Expression     = OrExpr AndExpr Expression
                    | Expr AndExpr

data AndExpr        = AndExpr NotExpr AndExpr
                    | NotAndExpr NotExpr

data NotExpr        = NotCompExpr CompareExpr
                    | NotExpr CompareExpr

data CompareExpr    = CompEqualsExpr AddExpr CompareExpr
                    | CompNotEqualExpr AddExpr CompareExpr
                    | CompGreaterExpr AddExpr CompareExpr
                    | CompGreaterEqExpr AddExpr CompareExpr
                    | CompLessExpr AddExpr CompareExpr
                    | CompLessEqExpr AddExpr CompareExpr
                    | CompExpr AddExpr

data AddExpr        = AddExpr MultExpr AddExpr
                    | SubAddExpr MultExpr AddExpr
                    | MultAddExpr MultExpr

data MultExpr       = MultExpr NegateExpr MultExpr
                    | DivMultExpr NegateExpr MultExpr
                    | NegMultExpr NegateExpr

data NegateExpr     = NegExpr PowerExpr
                    | PowNegExpr PowerExpr

data PowerExpr      = PowExpr Value PowerExpr
                    | ValPowExpr Value

data Value          = ParensVal Expression
                    | VarVal Var
                    | FxnVal Function
                    | ConstVal Constant

data Var            = IDVar ID

data Array          = IDArray ID [Expression]  -- ??

data Function       = INT Expression
                    | RND Expression

data Constant       = NumConst {num::Int}
                    | StringConst {str::String}

data ID             = ID {character::Char}

data Line_statement = Unparsed_line {line_num:: Int, unparsed:: String}
                    | Parsed_line {ix:: Int, origLine:: Int, sttment:: Statement}

data Interpreter = Program {s_table :: [(Char,Constant)], program_counter:: Int}

-------------------------------------------------------------
-- Derived instances for Data types                        --
-------------------------------------------------------------

instance Show IDList where
  show (IDList []) = "( )"
  show (IDList (id:ids)) = "(" ++ (show id) ++ (showCdr ids) ++ ")"

instance Show PrintList where
  show (PrintList e) = show e
  show (PrintListComma (e:es)) = (show e) ++ (showCdr es)
  show (PrintListSemiColon (e:es)) = (show e) ++ (showCdrSemi es)

showCdr [] = ""
showCdr (x:xs) = ", " ++ (showCdr xs)
showCdrSemi [] = ""
showCdrSemi (x:xs) = "; " ++ (showCdr xs)

instance Show Expression where
  show (OrExpr a e) = (show a) ++ " OR " ++ (show e)
  show (Expr a)     = show a

instance Show AndExpr where
  show (AndExpr n a) = (show n) ++ " AND " ++ (show a)
  show (NotAndExpr e) = show e

instance Show NotExpr where
  -- not at all clear we ever use the NotCompExpr constructor!
  show (NotCompExpr c) = " NOT(" ++ (show c) ++ ")"
  show (NotExpr c)     = show c

instance Show CompareExpr where
  show (CompEqualsExpr a c) = (show a) ++ " = " ++ (show c)
  show (CompNotEqualExpr a c) = (show a) ++ " <> " ++ (show c)
  show (CompGreaterExpr a c) = (show a) ++ " > " ++ (show c)
  show (CompGreaterEqExpr a c) = (show a) ++ " >= " ++ (show c)
  show (CompLessExpr a c) = (show a) ++ " < " ++ (show c)
  show (CompLessEqExpr a c) = (show a) ++ " <= " ++ (show c)
  show (CompExpr a) = show a

instance Show AddExpr where
  show (AddExpr m a) = (show m) ++ " + " ++ (show a)
  show (SubAddExpr m a) = (show m) ++ " - " ++ (show a)
  show (MultAddExpr m) = show m

instance Show MultExpr where
  show (MultExpr n m) = (show n) ++ " * " ++ (show m)
  show (DivMultExpr n m) = (show n) ++ " / " ++ (show m)
  show (NegMultExpr n) = show n

instance Show NegateExpr where
  show (NegExpr p) = "-" ++ (show p)
  show (PowNegExpr p) =  show p

instance Show PowerExpr where
  show (PowExpr v p) = (show v) ++ "^" ++ (show p)
  show (ValPowExpr v) = show v

instance Show Value where
  show (ParensVal e) = "(" ++ (show e) ++ ")"
  show (VarVal v) = show v
  show (FxnVal f) = show f
  show (ConstVal c) = show c

instance Show Var where
  show (IDVar (ID c)) = show (NoQuotesChar c)

instance Show Array where
  show (IDArray c []) = (show c) ++ "( )"
  show (IDArray c exprlist) = (show c) ++ "(" ++ (show exprlist) ++ ")"

instance Show Function where
  show (INT e) = "INT(" ++ (show e) ++ ")"
  show (RND e) = "RND(" ++ (show e) ++ ")"

instance Show Constant where
  show (NumConst n) = (show n)
  show (StringConst s) = show (NoQuotes s)

instance Show ID where
  show (ID c) = show c

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
  show (INPUT s idlist) = "INPUT " ++ (show s) ++ " " ++ (show idlist)
  show (LET x y) = "LET "   ++ (show x) ++ " = "    ++ (show y)
  show (NEXT x)  = "NEXT "  ++ (show x)
  show (PRINT e) = "PRINT " ++ (show e)
  show (END)     = "END"


newtype NoQuotes = NoQuotes String
newtype NoQuotesChar = NoQuotesChar Char

instance Show NoQuotes where
  show (NoQuotes showstr) = showstr
instance Show NoQuotesChar where
  show (NoQuotesChar char) = show (NoQuotes [char])
