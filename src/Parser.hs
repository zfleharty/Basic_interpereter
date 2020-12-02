module Parser where

import BasicTypes
import Parselib
import Data.Char
import Prelude hiding (LT,GT)
-- =========================================== --
--  Parsers for special individual characters  --
-- =========================================== --

equal = char ('=')
left  = char ('(')
right = char (')')

-- =========================================== --
--  Parsers for special key strings            --
-- =========================================== --

p_end :: Parser String
p_end = string "END"

p_goto :: Parser String
p_goto = string "GOTO"

p_for :: Parser String
p_for = string "FOR"

p_if :: Parser String
p_if = string "IF"

p_input :: Parser String
p_input = string "INPUT"

p_let :: Parser String
p_let = string "LET"

p_next :: Parser String
p_next = string "NEXT"

p_print :: Parser String
p_print = string "PRINT"

p_then :: Parser String
p_then = string "THEN"

p_to :: Parser String
p_to = string "TO"

p_int :: Parser String
p_int = string "INT"

p_rem :: Parser String
p_rem = string "REM"

p_rnd :: Parser String
p_rnd = string "RND"

-- at least one space
space1 :: Parser String
space1 = many1 (sat isSpace)

-- =========================================== --
--  Parsers for particular Statements          --
-- =========================================== --

statement       :: Parser Statement
end_statement   :: Parser Statement
for_statement   :: Parser Statement
if_statement    :: Parser Statement
input_statement :: Parser Statement
let_statement   :: Parser Statement
next_statement  :: Parser Statement
nextlist_statement :: Parser Statement
print_statement :: Parser Statement
rem_statement   :: Parser Statement


statement_list = [for_statement,input_statement,if_statement,let_statement,nextlist_statement,next_statement,
                 print_statement,rem_statement,end_statement,goto_statement]
statement = concatParsers statement_list
  


-- END
end_statement = do {_ <- token p_end; return END}

-- IF X = Y THEN 200
if_statement = do
  token p_if
  boolExpr <- token comp_expr
  token p_then
  c <- token p_const
  return (IF boolExpr c)



goto_statement = do
  token p_goto
  line <- nat
  return (GOTO line)


-- INPUT X
input_statement = do {
  token p_input;
  s <- todelim ';';
  token (char ';');
  var <- token p_var;
  return (INPUT s var)} +++ do {token p_input; var <- token p_var; return (INPUT "" var)}

-- FOR I = 1 TO H
for_statement = do
  token p_for
  var <- token p_var
  token equal
  fromExpr <- token expr
  token p_to
  toExpr <- token expr
  return (FOR var fromExpr toExpr)

-- LET X = Y
let_statement = do
  _ <- token p_let
  var <- token p_var
  _ <- token equal
  assigned <- token expr
  return (LET var assigned)

-- NEXT I or perhaps NEXT X, Y, Z?
next_statement = do
  token p_next
  var <- token p_var
  return (NEXT var)

-- NEXT X, Y, Z
nextlist_statement = do
  token p_next
  varlist <- token var_expr_list
  return (NEXTLIST varlist)

-- PRINT X
print_statement = do {token p_print; e <- print_list; return (PRINT e)}

print_list = expr_colon +++ expr_comma +++ expr

expr_colon = do
  e <- expr
  (token (char ';'))
  return (StringColon e)

expr_comma = do
  e <- expr
  token (char ',')
  return (StringComma e)
  
  

-- REM This is a comment
rem_statement = do
  token p_rem
  comment <- token (many item)
  return (REM comment)

-- =========================================== --
--  Parsers (and supporting fxns) for          --
--  Statement components                       --
-- =========================================== --

p_const  :: Parser Expression
p_number :: Parser Expression
p_var    :: Parser Expression
--p_symbol :: Parser Expression

var_char :: Char -> Bool
var_char_end :: Char -> Bool

var_char x     = isAlphaNum x || elem x "_"

var_char_end x = elem x "$%"

notAlphanum        :: Parser Char
notAlphanum         = sat (not.isAlphaNum)
  
p_const   = p_number -- +++ p_symbol

p_number = do {d <- token int; return (ConstExpr (realToFrac d))}

p_var    = do {var <- token upper; return (Var var)}
-- instead, make sure an upper case letter is followed by non-alpha char
-- to ensure we're only dealing with single-letter vars
-- p_var    = do {var <- upper; space1; return (Var var)}

--p_symbol = do {a <- sat (isAlpha); b <- many (sat var_char);
--               c <- many (sat var_char_end); return (StringConst (a:(b++c)))}


var_expr     :: Parser Expression
num_expr     :: Parser Expression
expr         :: Parser Expression
add_expr     :: Parser Expression
mult_expr    :: Parser Expression
int_fxn_expr :: Parser Expression
equals_expr  :: Parser CompareExpr

value        :: Parser Expression


var_expr = do {var <- token upper; return ((Var var))}
-- instead, can we make sure an upper case letter is followed by a
-- non-alphanumeric character, so we don't end up consuming the
-- the first letters of functions like INT or RND?
-- to ensure we're only dealing with single-letter vars
-- var_expr = do {var <- upper; notAlphanum; return (VarExpr (Var var))}

var_expr_list_cdr = do
  _ <- token (char ',')
  var <- token upper
  return [Var var]

-- var_expr_list = do
--   var <- token upper
--   possible_comma <- token item
--   if possible_comma == ','
--     then do
--       -- putStrLn $ "var = " ++ (show var)
--       varlisttail <- var_expr_list
--       return ((Var var):varlisttail)
--     else do
--       let varlisttail = []
--       return ((Var var):varlisttail)

var_expr_list :: Parser [Expression]
var_expr_list = do
  var <- token upper
  comma <- token (char ',')
  varlisttail <- var_expr_list +++ var_expr_list_last
  return ((Var var):varlisttail)

var_expr_list_last :: Parser [Expression]
var_expr_list_last = do
  var <- token upper
  return [Var var]

num_expr = do {d <- token int; return (ConstExpr (realToFrac d))}

-- fxn_expr = do {}

-- here try looking for int_fxn_expr FIRST so that the 1st letter(s)
-- not mistaken for variables?



------------------------------------------------------------------------------------
-- List of expressions to mappend together. When new expression parser is created --
-- add to this list definition to mappend it as part of the full expression type  --
-- parser                                                                         --
------------------------------------------------------------------------------------
expr_list = [comp_expr,add_expr,mult_expr,add_expr_paren,rnd_fxn_expr,int_fxn_expr,str_expr]

expr = concatParsers expr_list

comp_parsers = string <$> ["=","<>",">",">=","<","<="]



comp_expr = do                          
  e1 <- token add_expr
  o <- token $ concatParsers comp_parsers
  e2 <- token add_expr                  
  let op = case o of                             
        "=" -> (==) 
        "<>" -> (/=)
        ">" -> (>)
        ">=" -> (>=)
        "<" -> (<)
        "<=" -> (<=)
  return (Compare e1 e2 op)                  

str_expr = do
  string "\""
  s <- token $ todelim '\"'
  string "\""
  return (String' s)
  
add_expr  = do {
  x <- token mult_expr;
  token (char '+');
  y <- token add_expr;
  return (AddExpr x y)} +++ mult_expr

add_expr_paren = do {
  token (char '(');
  x <- token mult_expr;
  token (char '+');
  y <- token add_expr;
  token (char ')');
  return (AddExpr x y)} 

mult_expr = do {
  x <- token value;
  op <- token (sat (`elem` "/*"));
  y <- token mult_expr;
  return (case op of
      '*'-> (MultExpr x y)
      '/' -> (DivExpr x y))} +++ value

-- For the INT() situations
-- This and the rnd version might benefit from stricter linking
-- of the fxn name to following parenthesis
int_fxn_expr = do {
  token p_int;
  token (char '(');
  e <- token expr;
  token (char ')');
  return (FxnExpr "INT" e)
}

-- For the RND() situations
-- This and the INT version might benefit from stricter linking
-- of the fxn name to following parenthesis
rnd_fxn_expr = do {
  token p_rnd;
  token (char '(');
  e <- token expr;
  token (char ')');
  return (FxnExpr "RND" e)
}

-- set up for Expression = Expression
-- may be too general for our needs and rely on inadeq expr parser
equals_expr = do
  x <- token expr
  token equal
  y <- token expr
  return (CompEqualsExpr x y)

-- The arbitrary number of parens being consumed on either side
-- is problematic here for some situations, such as INT(2 * (3+4)).
-- See alternative further below.
-- value    = do{
--   many1 (char '(');
--   e <- expr;
--   many1 (char ')');
--   return e} +++  (num_expr) +++ (var_expr)

value    = do{
  char '(';
  e <- expr;
  char ')';
  return e } +++  rnd_fxn_expr +++ int_fxn_expr +++ (num_expr) +++ (var_expr)
