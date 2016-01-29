module skeleton8

/*
Done by Alexander Fedotov

The motivation for the "Map Ident Val" as a state is that I found it perfectly fitting here. I can work with both
integers and lists of integers in a convenient way. Indeed it is not the only option possible, but
other options are either ugly or a bit overhead.

Pretty printer is far from perfect, however it is straightforward to improve it (I just followed
the recomendation not to spend too much time on it).

*/

import StdList, StdInt, Data.Tuple, StdClass, iTasks._Framework.Generic, Text.JSON, Data.Functor, Control.Applicative, Control.Monad, Data.Void
import qualified iTasks
import qualified Text
from Text import class Text, instance Text String
from StdFunc import o
from StdTuple import fst
from Data.Map import :: Map, put, get, newMap
from Data.List import union, removeMember, instance Functor []
import qualified Data.List as List

:: Val = I Int | S [Int]
:: Ident :== String
:: Element :== Value Int //{sem :: Sem Int, print :: [String]}
:: Set :== Value [Int] //{sem :: Sem [Int], print :: [String]}
:: State :== Map Ident Val
:: Result a = Success (a, State) | Error String
:: Sem a = Sem (State -> Result a)
:: Value a = {sem :: Sem a, print :: [String]}

class read a where read :: Ident -> Sem a


instance read Int where
    read i = Sem \state -> case (get i state) of
                               Just (I x) = Success (x, state)
                               Just (S x) = Error ("Variable " +++ i +++ " has a wrong type")
                               otherwise = Error ("Variable " +++ i +++ " not found")

instance read [Int] where
    read i = Sem \state -> case (get i state) of
                               Just (S xs) = Success (xs, state)
                               Just (I x) = Error ("Variable " +++ i +++ " has a wrong type")
                               otherwise = Error ("Variable " +++ i +++ " not found")

class store a where store :: Ident a -> Sem a

instance store Int where
    store i x = Sem \state -> let newstate = put i (I x) state in Success (x, newstate)

instance store [Int] where
    store i xs = Sem \state -> let newstate = put i (S xs) state in Success (xs, newstate)

integer :: Int -> Element
integer i = {sem = return i, print = ["Int " +++ toString i]}

variable :: Ident -> Value a | read a
variable i = {sem = read i, print = ["Var " +++ i]}

size :: Set -> Element
size set = {sem = set.sem >>= \xs -> return (length xs), print = ["(Size ":set.print] ++ [")"]}

instance + Element where
    (+) x y = {sem = x.sem >>= \a -> y.sem >>= \b -> return (a + b), print = ["("] ++ x.print ++ ["+"] ++ y.print ++ [")"]}

instance - Element where
    (-) x y = {sem = x.sem >>= \a -> y.sem >>= \b -> return (a - b), print = ["("] ++ x.print ++ ["-"] ++ y.print ++ [")"]}

instance * Element where
    (*) x y = {sem = x.sem >>= \a -> y.sem >>= \b -> return (a * b), print = ["("] ++ x.print ++ ["*"] ++ y.print ++ [")"]}

insert :: Element Set -> Set
insert elem set = {sem = elem.sem >>= \x -> set.sem >>= \xs -> return ([x:xs]), print = ["(Insert "] ++ elem.print ++ elem.print ++ [")"]}

delete :: Element Set -> Set
delete elem set = {sem = elem.sem >>= \x -> set.sem >>= \xs -> return (filter (\a -> x <> a) xs), print = ["(Delete "] ++ elem.print ++ [")"]}

union :: Set Set -> Set
union set1 set2 = {sem = set1.sem >>= \xs -> set2.sem >>= \ys -> return (removeDup (xs ++ ys)), print = ["(Union "] ++ set1.print ++ set2.print ++ [")"]}

difference :: Set Set -> Set
difference set1 set2 = {sem = set1.sem >>= \xs -> set2.sem >>= \ys -> return ([x \\ x <- xs | not (isMember x ys)] ++ [x \\ x <- ys | not (isMember x xs)]), print = ["(Difference "] ++ set1.print ++ set2.print ++ [")"]}

intersection :: Set Set -> Set
intersection set1 set2 = {sem = set1.sem >>= \xs -> set2.sem >>= \ys -> return (removeDup ([x \\ x <- xs |  (isMember x ys)] ++ [x \\ x <- ys | (isMember x xs)])), print = ["(Intersection "] ++ set1.print ++ set2.print ++ [")"]}

(=.) infixl 2 :: Ident (Value a) -> Value a | store a
(=.) i x = {sem = x.sem >>= store i, print = ["(" +++ i +++ " =. "] ++ x.print ++ [")"]}

(==.) infix 4 :: (Value a) (Value a) -> Value Bool | == a
(==.) stmt1 stmt2 = {sem = stmt1.sem >>= \x -> stmt2.sem >>= \y -> return (x == y), print = ["("] ++ stmt1.print ++ [" == "] ++ stmt2.print ++ [")"]}

(<.) infix 4 :: (Value a) (Value a) -> Value Bool | < a
(<.) stmt1 stmt2 = {sem = stmt1.sem >>= \x -> stmt2.sem >>= \y -> return (x < y), print = ["("] ++ stmt1.print ++ [" < "] ++ stmt2.print ++ [")"]}

IF :: (Value Bool) THEN (Value a) ELSE (Value a) -> Value a
IF cond THEN stmt1 ELSE stmt2 = {sem = cond.sem >>= \b -> if b stmt1.sem stmt2.sem, print = ["(If"] ++ cond.print ++ ["then"] ++ stmt1.print ++ ["else"] ++ stmt2.print ++ [")"]}

WHILE :: (Value Bool) DO (Value a) -> Value Int
WHILE cond DO stmt = {sem = cond.sem >>= \b -> case b of
                                                   True = let w = stmt :. WHILE cond DO stmt in w.sem
                                                   otherwise = return 0, print = ["(While"] ++ cond.print ++ [" do "] ++ stmt.print ++ [")"]}

:: THEN = THEN
:: ELSE = ELSE
:: DO = DO

new :: Set
new = {sem = return [], print = ["New"]}

(:.) infixl 1 :: (Value a) (Value b) -> Value b
(:.) stmt1 stmt2 = {sem = stmt1.sem >>= \x -> stmt2.sem, print = ["("] ++ stmt1.print ++ [";"] ++ stmt2.print ++ [")"]}

instance Functor (Sem) where
    fmap :: (a -> b) (Sem a) -> (Sem b)
    fmap f (Sem g) = Sem func
    where
        func state = case g state of
                     Success (x, newstate) = Success (f x, newstate)
                     Error msg = Error msg


instance Applicative Sem where
    pure :: a -> Sem a
    pure x = Sem (\state -> Success (x, state))
    (<*>) infixl 4 :: (Sem (a -> b)) (Sem a) -> Sem b
    (<*>) (Sem f) (Sem g) = Sem func
    where
        func state = case f state of
                      Error msg = Error msg
                      Success (x, newstate) = case g newstate of
                                               Success (y, brandnewstate) = Success (x y, brandnewstate)
                                               Error msg = Error msg

instance Monad Sem where
    bind :: (Sem a) (a -> Sem b) -> Sem b
    bind (Sem f) g = Sem func
    where
        func state = case f state of
                      Error msg = Error msg
                      Success (x, newstate)
                          # (Sem h) = g x
                          = h newstate

print :: (Value a) -> String
print v = 'Text'.concat v.print

// examples

expr1 :: Element
expr1 = integer 2

expr2 :: Element
expr2 = expr1 + expr1

expr3 :: Element
expr3 = expr1 + expr1 * integer 3

expr4 :: Set
expr4 = difference (union (insert expr3 new) (delete (integer 8) (insert expr3 new))) (insert (integer 3) new)

expr5 :: Set
expr5 =
    x =. expr4 :.
    variable x

expr6 :: Element
expr6 =
    x =. insert (integer 11) new :.
    x =. size (variable x) :.
    variable x

expr7 :: Set
expr7 =
    x =. insert (integer 11) new :.
    y =. variable x

expr8 :: Set
expr8 =
    x =. insert (integer 11) new :.
    x =. insert (size (variable x)) (variable x) :.
    variable x

expr9 :: Set
expr9 =
    x =. insert (integer 0) new :.
    IF (size (variable x) ==. integer 0) THEN
        (x =. insert (integer 0) (variable x))
    ELSE
        (x =. delete (integer 0) (variable x)) :.
    variable x

expr10 :: Set
expr10 =
	z =. integer 7 :.
	x =. new :.
	x =. insert (variable z) (variable x) :.
	y =. union (variable x) (variable x) :.
	WHILE (size (variable x) <. integer 5) DO
		(x =. insert (size (variable x)) (variable x)) :.
	z =. difference (variable x) (intersection (variable x) (insert (variable z) new))

x = "x"
y = "y"
z = "z"

expr11 :: Element
expr11 = "x" =. integer 1 :. WHILE ((variable x) <. integer 9) DO ("x" =. variable x + integer 1)

eval :: (Value a) -> Result a
eval v = let (Sem f) = v.sem in f newMap

Start = (eval expr11, print expr11)
