{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Hw4 (Family(..),addPerson,addPeople,somebody,rmPerson,birthday,justJust,voting,getAge) where

{-
Family is a recursive data structure representing a family.
Each person in the family has a name, age, and a list of their children.
If they have no children, then that list will be empty.

For the purposes of this problem, everybody has only one parent,
  except for one person at the root of the family with no parent.

A Family is also allowed to have the value Nobody.  That value
will only be used when there is nobody in the family.

Here is an example of a Family:
Person "a" 20 [Person "d" 16 [],Person "b" 14 [Person "e" 10 [Person "f" 12 []],Person "c" 19 []],Person "g" 10 []]
In this family, a is the parent of d, b and g.  b is the parent of c and e.  e is the parent of f.
Also, a is 20 years old, etc.
Don't worry that the ages don't make logical sense in my example.
-}
type Name = String
type Age = Int
data Family = Nobody | Person Name Age [Family]
--  deriving Eq
  deriving Eq

{-
DONE!
Make Family an instance of Show
If Nobody is in the familly, it returns "There is nobody in the family"
If a Person has no children, it returns the name and age, separated by a colon
If a Person has children, it return the name and age (seaparated by colon), followed by a list of children
To use this: don't forget to delete undo "deriving Show" from above
The example from the first problem is shown as:
a:20[d:16,b:14[e:10[f:12],c:19],g:10]
-}
instance Show Family where
  show Nobody = "There is nobody in the family"
  show (Person n a []) = n ++ ":" ++ show a
  show (Person n a f) = n ++ ":" ++ show a ++ show f

{-
DONE!
addPerson adds a Person to the Family.
It takes a triple conatining: a person's name, their age and their parent
  and a Family
  and adds that Person to the Family in the proper place.

If the parent is not in the Family, the function does not add the Person

One exception is that if nobody is the Family, then that Person will become the only one in the Family
  and the given parent will be ignored.

Here are some examples.
See definitions of e1, e2, e3, e4 below:
*Hw4> e1
ann:1
*Hw4> e2
ann:1[bill:2]
*Hw4> e3
ann:1[chuck:3,bill:2]
*Hw4> e4
ann:1[chuck:3,bill:2[dave:4]]
*Hw4> addPerson ("ed",20,"fred") e4
ann:1[chuck:3,bill:2[dave:4]]
-}
addPerson :: (Name,Age,Name) -> Family -> Family
-- Your code goes here
addPerson (n,a,_) Nobody = Person n a []
addPerson trip@(n,a,p) (Person pn pa pf)
  | p == pn = Person pn pa $ Person n a [] : pf
  | otherwise = Person pn pa $ map (addPerson trip) pf

e1 :: Family
e1 = addPerson ("ann",1,"blah") Nobody
e2 :: Family
e2 = addPerson ("bill",2,"ann") e1
e3 :: Family
e3 = addPerson ("chuck",3,"ann") e2
e4 :: Family
e4 = addPerson ("dave",4,"bill") e3

{-
DONE!
addPeople takes a list of triples, as in the previous problem
and creates a Family with all those people, adding them in order from left to right
Example:
*Hw4> addPeople [("ann",1,"blah"),("bill",2,"ann"),("chuck",3,"ann"),("dave",4,"bill"),("ed",20,"fred")]
ann:1[chuck:3,bill:2[dave:4]]
-}
addPeople :: [(Name,Age,Name)] -> Family
-- Your code goes here
addPeople = foldl (flip addPerson) Nobody

{-
DONE!
somebody takes a list of families and returns a list of all families in that list that are not Nobody
Example:
*Hw4> somebody [e1,Nobody,e2,Nobody,e3]
[ann:1,ann:1[bill:2],ann:1[chuck:3,bill:2]]
-}
somebody :: [Family] -> [Family]
-- Your code goes here
somebody [] = []
somebody (Nobody:fs) = somebody fs
somebody (f:fs) = f : somebody fs

{-
DONE!
rmPerson takes a Name and a Family
It removes the Person with that name and all their descendants from the Family
Example:
*Hw4> rmPerson "bill" e4
ann:1[chuck:3]
*Hw4> rmPerson "ann" e4
There is nobody in this family
-}
rmPerson :: Name -> Family -> Family
rmPerson n Nobody = Nobody
rmPerson n (Person pn pa pf)
  | n == pn = Nobody
  | otherwise = Person pn pa $ filter (/=Nobody) $ map (rmPerson n) pf

{-
DONE!
birthday takes a Name and a Family
It adds 1 to the Age of the Person with that Name
If the Person is not in the Family it does nothing

Example:
*Hw4> birthday "bill" e4
ann:1[chuck:3,bill:3[dave:4]]
-}
birthday :: Name -> Family -> Family
birthday _ Nobody = Nobody
birthday n (Person pn pa pf)
  | pn == n = Person pn (pa + 1) pf
  | otherwise = Person pn pa $ map (birthday n) pf

{-
DONE!
voting takes a family as argument
it returns a list of the names of everybody in that Family who is at least 18 years old

Example:
*Hw4> voting e4
[]
-}
voting :: Family -> [Name]
voting Nobody = []
voting (Person pn pa []) = [pn | pa >= 18]
voting (Person pn pa pf) = [pn | pa >= 18] ++ concatMap voting pf

{-
DONE!
justJust takes a list of Maybe a's 
if everything in the list is Nothing it returns Nothing
  otherwise it returns the first element in the list that is not nothing
*Hw4> justJust [Nothing,Nothing,Just 2,Nothing,Just 4,Nothing]
Just 2
-}
justJust :: Eq a => [Maybe a] -> Maybe a
justJust [] = Nothing
justJust (maybe:maybes) = case maybe of
    (Just n) -> Just n
    Nothing -> justJust maybes

{-
DONE!
getAge takes a Name and a Family
If the Person with that Name is in the Family, it returns Just their age
Otherwise it returns Nothing

Example:
*Hw4> getAge "bill" e4
Just 2
*Hw4> getAge "fred" e4
Nothing
-}
getAge :: Name -> Family -> Maybe Age
getAge _ Nobody = Nothing
getAge n (Person pn pa pf)
  | n == pn = Just pa
  | otherwise = justJust $ map (getAge n) pf

e :: Family
e = addPeople [("a",20,""),("g",10,"a"),("b",14,"a"),("c",19,"b"),("d",16,"a"),("e",10,"b"),("f",12,"e")]

















