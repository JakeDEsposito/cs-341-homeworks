{-# LANGUAGE MultiWayIf #-}
module TestQ5 (run,stmt,evalStmt) where
import Debug.Trace
import Data.Char
import Text.ParserCombinators.ReadP
import ParseWS


{-
A statement is one of the following
  1. An if-else statement
     When we read it in, it is of the form:
       if (condition) statement else statement
  2. A while statement
     When we read it in, it is of the form:
       while (condition) statement
  3. An assignment statement
     When we read it in, it is of the form:
       variable = expression;
  4. A block of statements
     When we read it in, it is of the form:
       { statement statement ... statement }
     with zero or more statements in curly brackets
  5. A declaration of a variable
     When we read it in, it is of the form:
       int variable;
     so the only data type is integer
     A variable is initialized as zero when declared
     A variable is made up entirely of letters
-}
data Statement = IfElse Condition Statement Statement |
                 While Condition Statement |
                 Assign Expression Expression |
                 Block [Statement] |
                 Declare Expression
  deriving Show

{-
A condition is read in as one of the following forms:
  1. expression < expression
  2. expression > expression
  3. expression <= expression
  4. expression >= expression
  5. expression == expression
  6. expression != expression
  7. condition && condition
  8. condition || condition
  9. ! condition
Note:  Comparison operators have th highest precedence
  followed by "!", followed by "&&" followed by "||"
-}
data Condition = Less Expression Expression |
                 Greater Expression Expression |
                 LessEq Expression Expression |
                 GreaterEq Expression Expression |
                 Equal Expression Expression |
                 NotEqual Expression Expression |
                 And Condition Condition |
                 Or Condition Condition |
                 Not Condition
  deriving Show

{-
An expression is read in as one of the folowing forms:
  1. expression + expression
  2. expression - expression
  3. expression * expression
  4. expression / expression
  5. variable
  6. number
Note:  "*" and "/" have precedence over "+" and "-"
-}
data Expression = Plus Expression Expression |
                   Minus Expression Expression |
                   Times Expression Expression |
                   Divide Expression Expression |
                   Var String |
                   Num Int
  deriving Show

{-
Memory is a set of pairs consisting of
  - a variable
  - the current value of that variable
Variables could be duplicated in memory
  then I will assume the first occurrence
  of a variable gives the current value
-}
type Memory = [(String,Int)]

{-
This function will parse your input and run the program
A program is a list of statements surrounded by curly brackets
  in other words, a program is a statement
When you run your program, initially the memory is empty
This function will return the memory when the program is completed
-}

run :: String -> Memory
-- fill in your code here
run s = evalStmt (parsews stmt s) []

{-
To evaluate a statement you give
  1. the statement
  2. the current memory
It returns the memory after the statement is executed
-}
evalStmt :: Statement -> Memory -> Memory
--evalStmt stmt mem | trace ("evalStmt \n" ++ show stmt ++ "  " ++ show mem) False = undefined
-- fill in your code here
evalStmt (IfElse a b c) mem = if evalCond a mem then evalStmt b mem else evalStmt c mem
evalStmt stmt@(While a b) mem = if evalCond a mem then evalStmt stmt $ evalStmt b mem else mem
evalStmt (Assign (Var a) b) mem = (a,evalExp b mem) : mem
evalStmt (Assign _ b) mem = mem
evalStmt (Block []) mem = mem
evalStmt (Block (a:as)) mem = evalStmt (Block as) $ evalStmt a mem
evalStmt (Declare (Var a)) mem = (a,0):mem
evalStmt (Declare _) mem = mem

{-
To evaluate a condition you give
  1. the condition
  2. the current memory
It returns a bool indicating if the condition is true
-}
evalCond :: Condition -> Memory -> Bool
-- fill in your code here
evalCond (Less a b) mem = evalExp a mem < evalExp b mem
evalCond (Greater a b) mem = evalExp a mem > evalExp b mem
evalCond (LessEq a b) mem = evalExp a mem >= evalExp b mem
evalCond (GreaterEq a b) mem = evalExp a mem <= evalExp b mem
evalCond (Equal a b) mem = evalExp a mem == evalExp b mem
evalCond (NotEqual a b) mem = evalExp a mem /= evalExp b mem
evalCond (And a b) mem = evalCond a mem && evalCond b mem
evalCond (Or a b) mem = evalCond a mem || evalCond b mem
evalCond (Not a) mem = evalCond a mem

{-
To evaluate an expression you give
  1. the expression
  2. the current memory
It returns the value of the expression
-}
evalExp :: Expression -> Memory -> Int
-- fill in your code here
evalExp (Plus a b) mem = evalExp a mem + evalExp b mem
evalExp (Minus a b) mem = evalExp a mem - evalExp b mem
evalExp (Times a b) mem = evalExp a mem * evalExp b mem
evalExp (Divide a b) mem = evalExp a mem `div` evalExp b mem
evalExp (Var a) mem = snd $ head $ filter (\(var,_) -> var == a) mem
evalExp (Num a) mem = a

parseIfElse :: ReadP Statement
parseIfElse = do
  string "if"
  char '('
  cond <- cond
  char ')'
  stmt1 <- stmt
  string "else"
  IfElse cond stmt1 <$> stmt

parseWhile :: ReadP Statement
parseWhile = do
  string "while"
  char '('
  cond <- cond
  char ')'
  While cond <$> stmt

parseAssign :: ReadP Statement
parseAssign = do
  var <- parseVar
  char '='
  expr <- expr
  char ';'
  return (Assign var expr)

parseBlock :: ReadP Statement
parseBlock = do
  char '{'
  stmts <- many stmt
  char '}'
  return (Block stmts)

parseDeclare :: ReadP Statement
parseDeclare = do
  string "int"
  expr <- parseVar
  char ';'
  return (Declare expr)

-- This parses a statement and stores the result
stmt :: ReadP Statement
-- fill in your code here
stmt = parseIfElse <++ parseWhile <++ parseBlock <++ parseDeclare <++ parseAssign

parseLess :: ReadP Condition
parseLess = do
  expr1 <- expr
  char '<'
  Less expr1 <$> expr

parseGreater :: ReadP Condition
parseGreater = do
  expr1 <- expr
  char '>'
  Greater expr1 <$> expr

parseLessEq :: ReadP Condition
parseLessEq = do
  expr1 <- expr
  string ">="
  LessEq expr1 <$> expr

parseGreaterEq :: ReadP Condition
parseGreaterEq = do
  expr1 <- expr
  string "<="
  GreaterEq expr1 <$> expr

parseEqual :: ReadP Condition
parseEqual = do
  expr1 <- expr
  string "=="
  Equal expr1 <$> expr

parseNotEqual :: ReadP Condition
parseNotEqual = do
  expr1 <- expr
  string "!="
  NotEqual expr1 <$> expr

parseAnd :: ReadP Condition
parseAnd = do
  cond1 <- cond
  string "&&"
  And cond1 <$> cond

parseOr :: ReadP Condition
parseOr = do
  cond1 <- cond
  string "||"
  Or cond1 <$> cond

parseNot :: ReadP Condition
parseNot = do
  char '!'
  Not <$> cond

-- This parses a condition and stores the result
cond :: ReadP Condition
-- fill in your code here
cond = parseNotEqual <++ parseEqual <++ parseLessEq <++ parseGreaterEq <++ parseLess <++ parseGreater <++ parseNot <++ parseAnd <++ parseOr

parseVar :: ReadP Expression
parseVar = do
  var <- munch1 isAlpha
  return (Var var)

parseNum :: ReadP Expression
parseNum = do
  numChar <- munch1 isDigit
  let num = read numChar
  return (Num num)

-- This parses an exprssion and stores the result
expr :: ReadP Expression
-- fill in your code here
-- expr = parseVar <++ parseNum <++ parseDivide <++ parseTimes <++ parseMinus <++ parsePlus
expr = exprBase <++ parseVar <++ parseNum

exprBase :: ReadP Expression
exprBase = do
  expr1 <- parseVar <++ parseNum
  operator <- char '+' <++ char '-' <++ char '*' <++ char '/'
  expr2 <- parseVar <++ parseNum
  if
    | operator == '+' -> return (Plus expr1 expr2)
    | operator == '-' -> return (Minus expr1 expr2)
    | operator == '*' -> return (Times expr1 expr2)
    | operator == '/' -> return (Divide expr1 expr2)
    | otherwise -> error ("Unknown Operation: \"" ++ [operator] ++ "\"!")