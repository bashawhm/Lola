--Due Date: 2017-12-08 23:59:59.999999
module Run (run) where
import Debug.Trace
import Data.Char
import Parse


{-
A statement is one of the following
  1. An if-else statement
     When we read it in, it is of the form:
       if (condition) statement else statement
  2. A while statement
     When we read it in, it is of the form:
       while (condition) statement
  3. An assigment statement
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
You can assume that a condition will contain at most one
  boolean operator (&&,||,!)
So you don't have to worry about precedence or associativity
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
  then I will assume the first occurence
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
run code = evalStmt (parse stmt code) []

{-
To evaluate a statement you give
  1. the statement
  2. the current memory
It returns the memory after the statement is executed
-}
evalStmt (Block []) mem = mem
evalStmt (IfElse c s1 s2) mem = if (evalCond c mem) then (evalStmt s1 mem) else (evalStmt s2 mem)
evalStmt (While c s) mem = if (evalCond c mem) then (evalStmt (While c s) (evalStmt s mem)) else mem
evalStmt (Assign (Var v) e2) mem = ((v, (evalExp e2 mem)):mem)
evalStmt (Block (x:xs)) mem = evalStmt (Block xs) (evalStmt x mem)
evalStmt (Declare (Var s)) mem = ((s, 0):mem)

{-
To evaluate a condition you give
  1. the condition
  2. the current memory
It returns a bool indicating if the condition is true
-}
evalCond :: Condition -> Memory -> Bool
evalCond (Not c) mem = not (evalCond c mem)
evalCond (Or c1 c2) mem = (evalCond c1 mem ) || (evalCond c2 mem)
evalCond (And c1 c2) mem = (evalCond c1 mem) && (evalCond c2 mem)
evalCond (NotEqual e1 e2) mem = (evalExp e1 mem) /= (evalExp e2 mem)
evalCond (Equal e1 e2) mem = (evalExp e1 mem) == (evalExp e2 mem)
evalCond (GreaterEq e1 e2) mem = (evalExp e1 mem) >= (evalExp e2 mem)
evalCond (LessEq e1 e2) mem = (evalExp e1 mem) <= (evalExp e2 mem)
evalCond (Greater e1 e2) mem = (evalExp e1 mem) > (evalExp e2 mem)
evalCond (Less e1 e2) mem = (evalExp e1 mem) < (evalExp e2 mem)

{-
To evaluat an expression you give
  1. the expression
  2. the current memory
It returns the value of the expression
-}
evalExp :: Expression -> Memory -> Int
evalExp (Divide e1 e2) mem = (evalExp e1 mem) `div` (evalExp e2 mem)
evalExp (Times e1 e2) mem = (evalExp e1 mem) * (evalExp e2 mem)
evalExp (Minus e1 e2) mem = (evalExp e1 mem) - (evalExp e2 mem)
evalExp (Plus e1 e2) mem = (evalExp e1 mem) + (evalExp e2 mem)
evalExp (Var v) [] = error v
evalExp (Var v) mem = if v == fst (head mem) then snd (head mem) else evalExp (Var v) (tail mem)
evalExp (Num n) mem = n

ifElse :: Parse Char Statement
ifElse = (tokens "if(" <*< cond >*> token ')' <*< stmt >*> tokens "else" <*< stmt)
		`build` (\(c, (s1, s2)) -> (IfElse c s1 s2 ))


-- This parses a statement and stores the result
stmt :: Parse Char Statement
stmt = ifElse `alt` 
        ((tokens "while(" <*< cond >*> token ')' <*< stmt) `build` (\(c, s) -> (While c s))) `alt`
	     ((expr >*> token '=' <*< expr >*< token ';')
		 `build` (\(e1, e2) -> (Assign e1 e2))) `alt`
        ((token '{' <*< list(stmt) >*< token '}')
        `build` (\x -> Block x)) `alt` 
		((tokens "int" <*< expr >*< token ';') `build` (\x -> (Declare x)))


-- This parses a condition and stores the result
cond :: Parse Char Condition
cond = (condT `alt` cond'')

cond'' :: Parse Char Condition
cond'' = ((token '(' <*< cond' >*< token ')') `alt` cond')


cond' :: Parse Char Condition
cond' = ((expr >*> token '<' <*< expr) `build` (\(e1, e2) -> (Less e1 e2))) `alt`
        ((expr >*> token '>' <*< expr) `build` (\(e1, e2) -> (Greater e1 e2))) `alt`
        ((expr >*> tokens "<=" <*< expr) `build` (\(e1, e2) -> (LessEq e1 e2))) `alt`
        ((expr >*> tokens ">=" <*< expr) `build` (\(e1, e2) -> (GreaterEq e1 e2))) `alt`
		((expr >*> tokens "==" <*< expr) `build` (\(e1, e2) -> (Equal e1 e2))) `alt`
        ((expr >*> tokens "!=" <*< expr) `build` (\(e1, e2) -> (NotEqual e1 e2)))


condT :: Parse Char Condition
condT =  ((cond'' >*> tokens "&&" <*< cond) `build` (\(c1, c2) -> (And c1 c2))) `alt`
        ((cond'' >*> tokens "||" <*< cond) `build` (\(c1, c2) -> (Or c1 c2))) `alt`
        ((token '!' <*< cond) `build` (\c -> (Not c)))


var :: Parse Char Expression
var = (((((spot isAlpha) >*> (list (spot isAlphaNum))) `build` (uncurry(:)))) 
		`build` (\v -> (Var v))) 

varP :: Parse Char Expression
varP = ((((token '(' <*< (spot isAlpha) >*> (list (spot isAlphaNum)) >*< token ')') `build` (uncurry(:)))) 
		`build` (\v -> (Var v))) 
-- This parses an exprssion and stores the result
expr :: Parse Char Expression
expr = ((term >*> token '+' <*< expr) `build` (\(p1, p2) -> (Plus p1 p2))) `alt`
	    ((term >*> token '-' <*< expr) `build` (\(m1, m2) -> (Minus m1 m2))) `alt` term


term :: Parse Char Expression
term = ((fact >*> token '*' <*< term) `build` (\(t1, t2) -> (Times t1 t2))) `alt`
		((fact >*> token '/' <*< term) `build` (\(d1, d2) -> (Divide d1 d2))) `alt` fact

fact :: Parse Char Expression
fact = (var `alt` num `alt` varP)


num :: Parse Char Expression
num = (((((spot isDigit) >*> (list(spot isDigit))) `build` (uncurry(:)))) `build` (\n -> (Num (read n :: Int))))
