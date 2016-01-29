module assignment9

/*

Done by Alexander Fedotov

I used my dsl from week 7, but changed the state representation this time.
The adaptation to poor man's GADTs was relatively straightforward and static type checking works now.
I tried to make the iTasks simulation working, but I think it is impossible due to bimaps in the Expression data structure.

*/

from iTasks import always, hasValue, :: TaskValue(..), :: Task, :: Stability, :: TaskCont(..), :: Action, updateInformation, viewInformation, class descr, instance descr String, :: UpdateOption, :: ViewOption(..), -||-, -||, ||-, startEngine, class Publishable, >>*, class TFunctor, instance TFunctor Task, class TApplicative, instance TApplicative Task, instance Publishable Task, Void
import Data.Tuple, StdClass, StdList, iTasks._Framework.Generic, Text.JSON, Data.Functor, Control.Applicative, Control.Monad, Data.Map, StdTuple
import qualified iTasks
import qualified Text
from Text import class Text, instance Text String
from StdFunc import o
from Data.Map import :: Map, put, get, newMap

e1 :: Expression Element
e1 = int 3 //size (int 1)

:: BM a b = { t :: a -> b, f :: b -> a }
bm :: BM a a
bm = {f = id, t = id}

:: Expression a
	= New (BM a Set)
	| Insert (BM a Set)		 (Expression Element) (Expression Set)
	| Delete (BM a Set)		 (Expression Element) (Expression Set)
	| Variable	 Ident & read a
	| Union	(BM a Set)		 (Expression Set) (Expression Set)
	| Difference (BM a Set) (Expression Set) 	(Expression Set)
	| Intersection (BM a Set)	 (Expression Set) (Expression Set)
	| Integer	(BM a Element)	 Int
	| Size	(BM a Element)		 (Expression Set)
	| Oper	(BM a Element)		 (Expression Element) Op (Expression Element)
	| (=.)  infixl 2 Ident (Expression a) & store a

new = New bm
insert = Insert bm
delete = Delete bm
union = Union bm
difference = Difference bm
intersection = Intersection bm
int = Integer bm
size = Size bm
oper = Oper bm
var = Variable

:: Op 		=	+. | -. | *.
:: Set = S [Int]
:: Element = I Int
:: Ident	:== String

:: Val = Element | Set
:: Result a = Success (a, State) | Error String

// === State
:: State :== (Map Ident Element, Map Ident Set)

// === semantics
:: Sem a = Sem (State -> Result a)


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

class read a where read :: Ident -> Sem a

instance read Element where
    read i = Sem \state -> case (get i (fst state)) of
                               Just a = Success (a, state)
                               otherwise = Error ("Variable " +++ i +++ " not found")

instance read Set where
    read i = Sem \state -> case (get i (snd state)) of
                               Just a = Success (a, state)
                               otherwise = Error ("Variable " +++ i +++ " not found")

class store a where store :: Ident a -> Sem a

instance store Element where
    store i x = Sem \state -> let newstate = (put i x (fst state), snd state) in Success (x, newstate)

instance store Set where
    store i xs = Sem \state -> let newstate = (fst state, put i xs (snd state)) in Success (xs, newstate)

fail :: String -> Sem a
fail msg = Sem \state -> Error msg

eval :: (Expression a) -> Sem a
eval (New bm) = Sem \state -> Success (bm.f (S []), state)

eval (Insert bm e s) = eval e >>= \x -> eval s >>= \y -> let (I a) = x
                                                             (S b) = y in return (bm.f (S (removeDup [a:b])))
eval (Delete bm e s) = eval e >>= \x -> eval s >>= \y -> let (I a) = x
                                                             (S b) = y in return (bm.f (S (removeMember a b)))
eval (Variable i) = read i
eval (Union bm s1 s2) = eval s1 >>= \x -> eval s2 >>= \y -> let (S a) = x
                                                                (S b) = y in return (bm.f (S (removeDup (a ++ b))))
eval (Difference bm s1 s2) = eval s1 >>= \x -> eval s2 >>= \y -> let (S a) = x
                                                                     (S b) = y in return (bm.f (S ([x \\ x <- a | not (isMember x b)] ++ [x \\ x <- b | not (isMember x a)])))
eval (Intersection bm s1 s2) = eval s1 >>= \x -> eval s2 >>= \y -> let (S a) = x
                                                                       (S b) = y in return (bm.f (S (removeDup ([x \\ x <- a |  (isMember x b)] ++ [x \\ x <- b | (isMember x a)]))))
eval (Integer bm i) = return (bm.f (I i))
eval (Size bm s) = eval s >>= \x -> let (S a) = x in return (bm.f (I (length a)))
eval (Oper bm e1 +. e2) = eval e1 >>= \x -> eval e2 >>= \y -> let (I a) = x
                                                                  (I b) = y in return (bm.f (I (a + b)))
eval (Oper bm e1 -. e2) = eval e1 >>= \x -> eval e2 >>= \y -> let (I a) = x
                                                                  (I b) = y in return (bm.f (I (a - b)))
eval (Oper bm e1 *. e2) = eval e1 >>= \x -> eval e2 >>= \y -> let (I a) = x
                                                                  (I b) = y in return (bm.f (I (a * b)))
eval (i =. e) = eval e >>= \x -> store i x

evalExpr :: (Expression a) State -> Result a
evalExpr e state = res
    where
        res
         # (Sem f) = eval e
         = f state


print :: (Expression a) -> String
print e = 'Text'.concat (print` e [])
where
	print` :: (Expression a) [String] -> [String]
	print` (New bm) c = ["(New)": c]
	print` (Insert bm e s) c = ["(Insert ": print` e (print` s [")":c])]
	print` (Delete bm e s) c = ["(Delete ": print` e (print` s [")":c])]
	print` (Variable i) c = ["(Var " +++ i +++ ")": c]
	print` (Union bm s1 s2) c = ["(Union ": print` s1 (print` s2 [")":c])]
	print` (Difference bm s1 s2) c = ["(Difference ": print` s1 (print` s2 [")":c])]
	print` (Intersection bm s1 s2) c = ["(Intersection ": print` s1 (print` s2 [")":c])]
	print` (Integer bm i) c = ["(Int " +++ toString i +++ ")": c]
	print` (Size bm s) c = ["(Size ": print` s [")":c]]
	print` (Oper bm e1 +. e2) c = print` e1 [" + ": print` e2 c]
	print` (Oper bm e1 -. e2) c = print` e1 [" - ": print` e2 c]
	print` (Oper bm e1 *. e2) c = print` e1 [" * ": print` e2 c]
	print` (i =. e) c = [i +++ " = ": print` e c]


Start :: Result Element
Start = evalExpr e1 (newMap, newMap)

//Start = print e1
