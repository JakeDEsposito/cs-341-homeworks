module Hw2 (addb,munch,getValue,putValue,calcInt,calcSum,exec) where
import Debug.Trace

-- All these problems must be solved using recursion
-- Pattern matching is very useful for all of these problems, the more the better

{-
Digit is a digit in base 10, in other words an Int in [0..9]
-}
type Digit = Int
{-
Number is a list of digits representing a positive integer
  the list will be in reverse (i.e., from lower order to higher order digit)
  a number does not have leading zeroes
  [] represents zero
-}
type Number = [Digit]

{-
addb takes 3 arguments
  1. A Number (as described above)
  2. Another Number (as described above)
  3. A carry, which will always be 0 or 1
addb returns the result of adding the two positive integers plus the carry

Example:
addb [5,7] [9,8] 0
[4,6,1]

-}
addb :: Number -> Number -> Digit -> Number
-- Fill in your code here
addb [] [] 0 = []
addb [] [] 1 = [1]
addb (x:xs) (y:ys) d = (current_num - (carry * 10)) : addb xs ys carry
  where
    current_num = x + y + d
    carry = if current_num >= 10 then 1 else 0
--addb xs ys d = []


{-
DONE!!!

munch takes 2 arguments
  1. A character ch 
  2. A string
munch returns a pair
- whose first component is a string consisting of all the c's at the beginning of the string
- whose second component is the rest of the string
Examples:
munch 'a' "aaabbaa"
("aaa","bbaa")
munch 'a' "aaaa"
("aaaa","")
munch 'b' "aabb"
("","aabb")

-}
munch :: Char -> String -> (String,String)
-- Fill in your code here
munch _ "" = ("", "")
munch ch (c:cs) = if ch == c then ([c] ++ (fst next_step), (snd next_step)) else ([], [c] ++ cs)
  where
    next_step = munch ch cs

--munch ch cs = ("","")



{-
DONE!!!

putValue takes 3 arguments
  1. A Memory, as in the previous assignment
  2. The name of a variable x
  3. An Int m
putValue should assign x to m and return the updated memory
This is the same function as in the previous homework except:
- You must solve this using recursion
- If x doesn't exist, create it and assign it m
Examples:
mem = [("x",4),("y",6),("z",8)]
putValue mem "y" 12
[("x",4),("y",12),("z",8)]
putValue mem "w" 10
[("x",4),("y",6),("z",8),("w",10)]

-}
putValue :: Memory -> String -> Int -> Memory
-- Fill in your code here
putValue [] x m = [(x, m)]
putValue (a:mem) x m
  | fst a == x = (x, m) : mem
  | otherwise = a : putValue mem x m

{-
DONE!!!

getValue takes 2 arguments
  1. A Memory, as in the previous assignment
  2. The name of a variable x
getValue returns the value of x
This is the same function as in the previous homework except:
- You must solve this using recursion
- It should raise an exception if x does not exist
  But if x has multiple values just return the first one
Examples:
mem = [("x",4),("y",6),("z",8)]
getValue mem "y"
6
getValue mem "w"
*** Exception: Cannot find "w"
CallStack (from HasCallStack):
  error, called at Hw2.hs:156:17 in main:Hw2

-}
getValue :: Memory -> String -> Int
-- Fill in your code here
getValue [] _ = error "Not Defined"
getValue (m:mem) x = if (fst m) == x then snd m else getValue mem x

{-
DONE !!!

calcInt takes 2 arguments
  1. An Int x
  2. A list of pairs with 
  - first component is the character '+' or '*
  - second component is an Int
Starting with x, calcInt goes through the list 
- if the first component is '+', it adds the second component
- if the first component is '*', it multiplies the second component
- otherwise it throws an exception
Examples:
calcInt 4 [('+',1)]       
5
calcInt 4 [('+',1),('*',2)]
10
calcInt 4 [('+',1),('*',2),('+',8)]
18

-}
calcInt :: Int -> [(Char,Int)] -> Int
-- Fill in your code here
calcInt x [] = x
calcInt x (p:ps)
  | (fst p) == '+' = calcInt ((snd p) + x) ps
  | (fst p) == '*' = calcInt ((snd p) * x) ps
  | otherwise = error "Invalid Operator"

{-
DONE!!!

calcSum takes 2 arguments
  1. A Memory
  2. A list of triples of 3 variable names 
Starting with the initial memory, calcSum goes through the list 
- If the triple is ("x","y","z") it assigns x the value y+z
You will want to call putValue and getValue
You should not call calcInt
Using where clauses is helpful
Examples:
mem = [("x",4),("y",6),("z",8)]
calcSum mem [("z","x","y")]
[("x",4),("y",6),("z",10)]
calcSum mem [("z","x","y"),("y","x","z")]
[("x",4),("y",14),("z",10)]

-}
calcSum :: Memory -> [(String,String,String)] -> Memory
-- Fill in your code here
calcSum mem [] = mem
calcSum mem ((x, y, z):ps) = if not (null ps) then calcSum new_mem ps else new_mem
  where
    yVal = getValue mem y
    zVal = getValue mem z
    new_mem = putValue mem x (yVal + zVal)

{-
Statement is an assembly language statement with three parts
  1. an instruction i
  2. a variable name v
  3. an integer n
Program is a list of Statements, to be executed in order
  implicitly think of each statement in the program to have a line number
  with the first instruction at line number 0
Memory represents the memory of the computer
  Memory is a list of pairs of a variable and its assigned value
The meaning of each instruction is as follows:
  1. load v n
    give variable v the value n in memory
  2. add v n
    add n to the value of v in memory
  3. jmp v n
    go to line number n, note that v is ignored so anything is allowed
  4. blz v n
    if the value of v is <= 0 then go line n
      otherwise proceed to the next line in the program
  5. ret v n
    quit the program and return the value v, here n is ignored
-}
type Inst = String
type Variable = String
type Statement = (Inst,Variable,Int)
type Program = [Statement]
type Memory = [(Variable,Int)]


{-
DONE!!!

exec executes your program (or a part of your program)
  and returns its return value
A program is executed by executing each instruction in order, except for jmp or blz
exec takes as parameters:
  1. The entire program
  2. The piece of the program that is currently being evaluated
    (i.e., the current instruction up to the end of the program)
It returns the result of the first return statement it encounters
Note 1: Ise putValue and getValue but not other functions
Note 2: I don't care what you do if there are errors, such as:
  1. syntax errors
  2. jumping out of the program
  3. not encountering a return statement
Your function will actually be an interpreter
  - It will take a partial program and a list of memory values and execute the first line of the program
  - Then call itself recursively on a new program and a new list of memory values
This problem is much easier if you use pattern matching

 exec prog1 prog1 []
9

-}
exec :: Program -> Program -> Memory -> Int
-- Uncomment the following line if you want to trace your code
-- But re-comment it before you turn in your code
--exec prog cur mem | trace ("exec " ++ show  (head cur) ++ "  " ++ show mem) False = undefined 
-- Fill in your code here

exec prog (("load", x, val):partial) mem = exec prog partial (putValue mem x val)
exec prog (("add", x, new_val):partial) mem = exec prog partial (putValue mem x ((getValue mem x) + new_val))
exec prog (("jmp", _, num):_) mem = exec prog (drop num prog) mem
exec prog (("blz", v, x):partial) mem = exec prog (if (getValue mem v) <= 0 then (drop x prog) else partial) mem
exec prog (("ret", x, _):_) mem = getValue mem x
--exec prog ((instruction, _, _):_) _ | trace ("ERROR: " ++ (show instruction) ++ " is not a valid instruction!") False = undefined
exec prog partial mem = 0

{-
Initially calling exec function

test_exec prog1
9

-}
test_exec :: Program -> Int
--No need to fill in code here
test_exec p = exec p p []


{-
Example program
-}
prog1 :: Program
prog1 = [("load","x",4),("load","y",5),("load","z",0),
         ("blz","y",7),("add","z",1),("add","y",(-1)),("jmp","",3),
         ("blz","x",11),("add","z",1),("add","x",(-1)),("jmp","",7),("ret","z",0)]