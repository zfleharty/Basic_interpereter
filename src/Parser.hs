module Parser where

import BasicTypes
import Parselib
import Data.Char
import Prelude hiding (LT,GT)
-- =========================================== --
--  Parsers for special individual characters  --
-- =========================================== --

equal   :: Parser Char
left    :: Parser Char
right   :: Parser Char
equal   = char ('=')
left    = char ('(')
right   = char (')')

-- =========================================== --
--  Parsers for special key strings            --
-- =========================================== --

p_end    :: Parser String
p_goto   :: Parser String
p_for    :: Parser String
p_if     :: Parser String
p_input  :: Parser String
p_let    :: Parser String
p_next   :: Parser String
p_print  :: Parser String
p_to     :: Parser String
p_int    :: Parser String
p_rem    :: Parser String
p_rnd    :: Parser String
space1   :: Parser String
p_then   :: Parser String
p_gosub  :: Parser String
p_return :: Parser String
p_tab    :: Parser String
parensed :: Parser a -> Parser a 

p_end    = string "END"
p_goto   = string "GOTO"
p_for    = string "FOR"
p_if     = string "IF"
p_input  = string "INPUT"
p_let    = string "LET"
p_next   = string "NEXT"
p_not    = string "NOT"
p_print  = string "PRINT"
p_then   = string "THEN"
p_to     = string "TO"
p_int    = string "INT"
p_rem    = string "REM"
p_tab    = string "TAB"
p_rnd    = string "RND"
p_gosub  = string "GOSUB"
p_return = string "RETURN"

space1   = many1 (sat isSpace)
parensed p = do {token $ char '('; e <- p; token $ char ')'; return e}

-- =========================================== --
--  Parsers for particular Statements          --
-- =========================================== --
statement_list        :: [Parser Statement]
statement             :: Parser Statement
end_statement         :: Parser Statement
for_statement         :: Parser Statement
if_statement          :: Parser Statement
input_statement       :: Parser Statement
input_multi_statement :: Parser Statement
let_statement         :: Parser Statement
next_statement        :: Parser Statement
--nextlist_statement    :: Parser Statement
print_statement       :: Parser Statement
rem_statement         :: Parser Statement


statement_list = [for_statement,input_multi_statement,input_statement,if_statement,let_statement_alt,
                 print_statement,rem_statement,end_statement,goto_statement,
                 next_statement,gosub_statement,return_statement,dim_statement,
                 assignment_statement, on_statement,for_step_statement]

statement      = concatParsers statement_list
  


-- END
end_statement = do {_ <- token p_end; return END}

-- IF X = Y THEN 200
if_statement = do
  token p_if
  boolExpr <- token and_expr
  token p_then
  c <- token p_const
  return (IF boolExpr c)

-- GOTO 200
goto_statement = do
  token p_goto
  line <- nat
  return (GOTO line)

return_statement = do
  p_return
  return RETURN

gosub_statement = do
  token p_gosub
  line <- nat
  return (GOSUB line)

-- INPUT X
input_statement = do {
  token p_input;
  s <- todelim ';';
  token (char ';');
  var <- token p_id;
  return (INPUT s var)} +++ do {token p_input; var <- token p_id; return (INPUT "" var)}

-- INPUT "ENTER INPUT: "; X, Y
-- klunky effort to accommodate multi-input INPUT statements
input_multi_statement = do
  token p_input
  s <- todelim ';'
  token (char ';')
  var1 <- token p_id
  token (char ',')
  var2 <- token p_id
  return (INPUTMULTI s [var1, var2])

-- FOR I = 1 TO H
for_statement = do
  token p_for
  var <- token p_id
  token equal
  fromExpr <- token expr
  token p_to
  toExpr <- token expr
  return (FOR var fromExpr toExpr)

for_step_statement = do
  token p_for
  var <- token p_id
  token equal
  fromExpr <- token expr
  token p_to
  toExpr <- token expr
  token $ string "STEP"
  step <- token expr
  return (FORSTEP var fromExpr toExpr step)

-- NEXT I or perhaps NEXT X, Y, Z?
next_statement = do
  token p_next
  ids <- token id_list
  return (NEXT ids)

-- -- NEXT X, Y, Z
-- nextlist_statement = do
--   token p_next
--   varlist <- token var_expr_list
--   return (NEXTLIST varlist)

-- LET X = Y
let_statement = do
  _ <- token p_let
  var <- token p_id
  _ <- token equal
  assigned <- token expr
  return (LET var assigned)

-- ALTERNATIVE let_statement trying to accommodate more general
-- LET <Variable> = <Expression>
-- where the <Variable> could be an array specifier
let_statement_alt = do
  _ <- token p_let
  var <- token variable  -- but what to do here?
  _ <- token equal
  assigned <- token expr
  return (LET var assigned)

assignment_statement = do {
  c <- token $ variable;
  token $ char '=';
  e <- token $ expr;
  return $ ASSIGNMENT c e
  }
on_statement = do {
  token $ string "ON";
  e <- token expr;
  token p_goto;
  ids <- token integer_list;
  return $ ON e ids
  }

--------------------------------------------------------------
-- Need to create parsers to mimic this subset of grammar   --
-- ID             = {letter}                                CHECK--
-- ID List        = ID , <ID List>                          CHECK
--                | ID                                      CHECK
-- DIM <Array List>                                         --
-- <Array List>      ::= <Array> ',' <Array List>           CKECH--
--                     | <Array>                            CHECK--
-- <Array>       ::= ID '(' <Expression List> ')'           CHECK--
-- <Expression List> ::= <Expression> ',' <Expression List> CHECK--
--                     | <Expression>                       CHECK--
--------------------------------------------------------------
         
concatExpr ls rec = rec <$> ls

id_list = do{
  i <- token p_id;
  token $ char ',';
  ids <- id_list;
  return $ case ids of
      (IDList ids') -> (IDList $ i:(ids'))
      (Var i')      -> (IDList $ i:[(Var i')])
  } +++ p_id

integer_list = do {
  i <- nat;
  token $ char ',';
  is <- integer_list;
  return $ i:is} +++ do{i <- nat; return [i]}

dim_statement = do {token $ string "DIM"; as <- array_list; return $ DIM as}

-----------------------------------------
-- list_expression ep cons = do {      --
--   e <- ep;                          --
--   token $ char ',';                 --
--   es <- list_expression ep cons;    --
--   return $ case es of               --
--       (cons es') -> (cons $ e:es')  --
--       e'         -> (cons $ e:[e']) --
--   } +++ ep                          --
-----------------------------------------
  
-- X(A, B), Y(C, D)
array_list = do {
  a <- array';
  token $ char ',';
  as <- array_list;
  return $ case as of
      (ArrayList as') -> (ArrayList $ a:as')
      a'              -> (ArrayList $ a:[a'])
                } +++ array'

variable = do {
  i <- p_id;
  es <- parensed expr_list;
  return $ Array (id' i) es
} +++ p_id

-- X(A, B)
-- why array' instead of just array?
array' = do {
  i <- p_id;                         -- i is something like Var 'X'
  es <- parensed expr_list;         
  return $ Array (id' i) es      -- id' i returns the 'X' part
           }

expr_list = do {
  e <- expr;
  token $ char ',';
  es <- expr_list;
  return $ case es of
      (ExpressionList es') -> (ExpressionList $ e:es')
      e'                   -> (ExpressionList $ e:[e'])
  } +++ expr


-- PRINT X
print_statement = do {token p_print; e <- print_list; return (PRINT e)} +++ do {token p_print; return (PRINT [])}

print_list = expr_colon_tab +++ expr_colon +++ expr_comma +++ single_expr 

single_expr = do
  e <- expr
  return [e]
  
expr_colon = do{
  e <- expr;
  (token (char ';'));
  es <- print_list;
  return $ (StringColon e):es} +++ do {e <- expr; (token (char ';')); return $ [StringComma e]}

expr_comma = do {
  e <- expr;
  token (char ',');
  es <- print_list;
  return $ (StringComma e):es} +++ do {e <- expr; (token (char ',')); return $ [StringComma e]}
  
-- Tried to add a TAB-related parser at the expr level
-- (see tab_print_expr) further down, but it refused to cooperate and
-- "catch" the "TAB" string, so added an explicit parser here to catch
-- the "TAB(#);" scenrio and add it to print_list above.
expr_colon_tab = do
  token (string "TAB(")
  n <- token nat
  token (string ");")
  let s = replicate (n-1) ' '
  es <- print_list
  return $ (StringColon (String' s)):es

-- REM This is a comment
rem_statement = do
  token p_rem
  comment <- token (many item)
  return (REM comment)

-- =========================================== --
--  Parsers (and supporting fxns) for          --
--  Statement components                       --
-- =========================================== --

p_const      :: Parser Expression
p_number     :: Parser Expression
p_id        :: Parser Expression
var_char     :: Char -> Bool
var_char_end :: Char -> Bool
num_expr     :: Parser Expression
expr         :: Parser Expression
add_expr     :: Parser Expression
mult_expr    :: Parser Expression
int_fxn_expr :: Parser Expression
and_expr     :: Parser Expression
value        :: Parser Expression
notAlphanum  :: Parser Char


var_char x     = isAlphaNum x || elem x "_"

var_char_end x = elem x "$%"

notAlphanum    = sat (not.isAlphaNum)

p_const        = p_number 

p_number       = do {d <- token int; return (ConstExpr (realToFrac d))}

p_id          = do {var <- token upper; return (Var var)}

num_expr       = do {d <- token int; return (ConstExpr (realToFrac d))}




-- instead, can we make sure an upper case letter is followed by a
-- non-alphanumeric character, so we don't end up consuming the
-- the first letters of functions like INT or RND?
-- to ensure we're only dealing with single-letter vars
-- var_expr = do {var <- upper; notAlphanum; return (VarExpr (Var var))}
var_expr_list_cdr = do
  _ <- token (char ',')
  var <- token upper
  return [Var var]


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


------------------------------------------------------------------------------------
-- List of expressions to mappend together. When new expression parser is created --
-- add to this list definition to mappend it as part of the full expression type  --
-- parser                                                                         --
------------------------------------------------------------------------------------
expr_list' = [tab_print_expr, and_expr, not_expr, comp_expr, add_expr, mult_expr, 
             rnd_fxn_expr, int_fxn_expr, str_expr]

expr = concatParsers expr_list'



expr' = do {
  e1 <- token and_expr;
  token $ string "OR";
  e2 <- token expr';
  return $ OrExpr e1 e2} +++ and_expr

and_expr = do {
  e1 <- token not_expr;
  token (string "AND");
  e2 <- token and_expr;
  return (AndExpr e1 e2)} +++ not_expr

not_expr = do {
  token p_not;
  e <- token comp_expr;
  return (NotExpr e)} +++ comp_expr


comp_expr = do {                  
  e1 <- token add_expr;
  o <- concatParsers $ string <$> ["=","<>","<=","<",">=",">"];
  e2 <- token comp_expr;                  
  return (Compare e1 e2 o)} +++ add_expr

add_expr  = do {
  x <- token mult_expr;
  op <- token (sat (`elem` "+-"));
  y <- token add_expr;
  return (case op of
            '+' -> AddExpr x y
            '-' -> SubExpr x y)} +++ mult_expr

mult_expr = do {
  x <- token value;
  op <- token (sat (`elem` "/*"));
  y <- token mult_expr;
  return (case op of
      '*'-> (MultExpr x y)
      '/' -> (DivExpr x y))} +++ value

value = (parensed expr) +++  function_expr +++ (num_expr) +++ variable +++ (p_id)




function_expr = rnd_fxn_expr +++ int_fxn_expr


str_expr = do
  string "\""
  s <- token $ todelim '\"'
  string "\""
  return (String' s)

-- the tab_print_expr doesn't get "caught" somehow, still
-- allowing the "T" from the "TAB" to get peeled off first.
-- Added a expr_colon_tab parser higher up in the chain close to the
-- print_statement parser which (for now) avoids the extra expr level
tab_print_expr = do
  token p_tab
  n <- parensed nat
  -- token (char '(');
  -- n <- nat
  -- token (char ')')
  let s = replicate (n - 1) ' '
  return (String' s)
  

-- For the INT() situations
-- This and the rnd version might benefit from stricter linking
-- of the fxn name to following parenthesis
int_fxn_expr = do {
  token p_int;
  e <- parensed expr;
  return (FxnExpr "INT" e)
}

-- For the RND() situations
-- This and the INT version might benefit from stricter linking
-- of the fxn name to following parenthesis
rnd_fxn_expr = do {
  token p_rnd;
  e <- parensed expr;
  return (FxnExpr "RND" e)
}





