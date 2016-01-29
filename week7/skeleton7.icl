module skeleton7

from iTasks import always, hasValue, :: TaskValue(..), :: Task, :: Stability, :: TaskCont(..), :: Action, updateInformation, viewInformation, class descr, instance descr String, :: UpdateOption, :: ViewOption(..), -||-, -||, ||-, startEngine, class Publishable, >>*, class TFunctor, instance TFunctor Task, class TApplicative, instance TApplicative Task, instance Publishable Task, Void
import Data.Tuple, StdClass, StdList, iTasks._Framework.Generic, Text.JSON, Data.Functor, Control.Applicative, Control.Monad, Data.Map, StdTuple
import qualified iTasks
import qualified Text
from Text import class Text, instance Text String
from StdFunc import o

e = Insert New (Oper New +. (Union (Integer 7) (Size (Integer 9))))

:: Expression
	= New
	| Insert		 Element Set
	| Delete		 Element Set
	| Variable		 Ident
	| Union			 Set 	Set
	| Difference  	 Set 	Set
	| Intersection	 Set 	Set
	| Integer		 Int
	| Size			 Set
	| Oper			 Element Op Element
	| (=.)  infixl 2 Ident Expression

:: Op 		=	+. | -. | *.
:: Set 		:== Expression
:: Element	:== Expression
:: Ident	:== String

:: Val = I Int | S [Int]
:: Result a = Success (a, State) | Error String

// === State
:: State :== [(Ident, Val)]

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

put :: Ident Val State -> State
put i v [] = [(i, v)]
put i v [x:xs] = if (i == fst x) [(i, v):xs] [x: put i v xs]

get :: Ident State -> Maybe Val
get i [] = Nothing
get i [x:xs] = if (i == fst x) (Just (snd x)) (get i xs)

store :: Ident Val -> Sem Val
store i v = Sem \state -> Success (v, put i v state)

read :: Ident -> Sem Val
read i = Sem \state -> case get i state of Just x = Success (x, state); otherwise = Error ("Undeclared variable: " +++ i)

fail :: String -> Sem a
fail msg = Sem \state -> Error msg

eval :: Expression -> Sem Val
eval New = Sem \state -> Success (S [], state)
eval (Insert e s) = eval e >>= \x -> case x of
                                      (I a) = eval s >>= \y -> case y of
                                                                (I b) = fail "Set expected instead of an element: Insert e ^s"
                                                                (S b) = return (S (removeDup [a:b]))
                                      (S a) = fail "Element expected instead of a set: Insert ^e s"
eval (Delete e s) = eval e >>= \x -> case x of
                                      (I a) = eval s >>= \y -> case y of
                                                                (I b) = fail "Set expected instead of an element: Insert e ^s"
                                                                (S b) = return (S (removeMember a b))
                                      (S a) = fail "Element expected instead of a set: Insert ^e s"
eval (Variable i) = read i
eval (Union s1 s2) = eval s1 >>= \x -> case x of
                                        (I a) = fail "Set expected instead of an element: Union ^x y"
                                        (S a) = eval s2 >>= \y -> case y of
                                                                   (I b) = fail "Set expected instead of an element: Union x ^y"
                                                                   (S b) = return (S (removeDup (a ++ b)))
eval (Difference s1 s2) = eval s1 >>= \x -> case x of
                                            (I a) = fail "Set expected instead of an element: Difference ^x y"
                                            (S a) = eval s2 >>= \y -> case y of
                                                                       (I b) = fail "Set expected instead of an element: Difference x ^y"
                                                                       (S b) = return (S ([x \\ x <- a | not (isMember x b)] ++ [x \\ x <- b | not (isMember x a)]))
eval (Intersection s1 s2) = eval s1 >>= \x -> case x of
                                              (I a) = fail "Set expected instead of an element: Intersection ^x y"
                                              (S a) = eval s2 >>= \y -> case y of
                                                                        (I b) = fail "Set expected instead of an element: Intersection x ^y"
                                                                        (S b) = return (S (removeDup ([x \\ x <- a |  (isMember x b)] ++ [x \\ x <- b | (isMember x a)])))
eval (Integer i) = return (I i)
eval (Size s) = eval s >>= \x -> case x of
                                  (I a) = fail "Set expected instead of an element: Size ^x"
                                  (S a) = return (I (length a))
eval (Oper e1 +. e2) = eval e1 >>= \x -> case x of
                                          (S a) = fail "Element expected instead of a set: Oper ^x +. y"
                                          (I a) = eval e2 >>= \y -> case y of
                                                                     (S b) = fail "Element expected instead of a set: Oper x +. ^y"
                                                                     (I b) = return (I (a + b))
eval (Oper e1 -. e2) = eval e1 >>= \x -> case x of
                                          (S a) = fail "Element expected instead of a set: Oper ^x -. y"
                                          (I a) = eval e2 >>= \y -> case y of
                                                                     (S b) = fail "Element expected instead of a set: Oper x -. ^y"
                                                                     (I b) = return (I (a - b))
eval (Oper e1 *. e2) = eval e1 >>= \x -> case x of
                                          (S a) = fail "Element expected instead of a set: Oper ^x *. y"
                                          (I a) = eval e2 >>= \y -> case y of
                                                                     (S b) = fail "Element expected instead of a set: Oper x *. ^y"
                                                                     (I b) = return (I (a * b))
eval (i =. e) = eval e >>= \x -> store i x

evalExpr :: Expression State -> Result Val
evalExpr e state = res
    where
        res
         # (Sem f) = eval e
         = f state

print :: Expression -> String
print e = 'Text'.concat (print` e [])
where
	print` :: Expression [String] -> [String]
	print` New c = ["(New)": c]
	print` (Insert e s) c = ["(Insert ": print` e (print` s [")":c])]
	print` (Delete e s) c = ["(Delete ": print` e (print` s [")":c])]
	print` (Variable i) c = ["(Var " +++ i +++ ")": c]
	print` (Union s1 s2) c = ["(Union ": print` s1 (print` s2 [")":c])]
	print` (Difference s1 s2) c = ["(Difference ": print` s1 (print` s2 [")":c])]
	print` (Intersection s1 s2) c = ["(Intersection ": print` s1 (print` s2 [")":c])]
	print` (Integer i) c = ["(Int " +++ toString i +++ ")": c]
	print` (Size s) c = ["(Size ": print` s [")":c]]
	print` (Oper e1 +. e2) c = print` e1 [" + ": print` e2 c]
	print` (Oper e1 -. e2) c = print` e1 [" - ": print` e2 c]
	print` (Oper e1 *. e2) c = print` e1 [" * ": print` e2 c]
	print` (i =. e) c = [i +++ " = ": print` e c]


// === simulation
(>>>=)     :== 'iTasks'.tbind
(>>>|) a b :== 'iTasks'.tbind a (\_ -> b)
treturn    :== 'iTasks'.return
ActionOk   :== 'iTasks'.ActionOk
ActionQuit :== 'iTasks'.ActionQuit
ActionNew  :== 'iTasks'.ActionNew

derive class iTask Val, Expression, Op

simulate :: Expression State String -> Task Expression
simulate e state res = (viewInformation "State:" [] state ||- (viewInformation "Result:" [] res ||- (viewInformation "Expression:" [] (print e) ||- updateInformation "Current statement:" [] e)))
               >>* [OnAction ActionOk (hasValue (\e -> case evalExpr e state of Success (x, newstate) = simulate e newstate (toString x); Error msg = simulate e state ("Error: " +++ msg)))]

instance toString Val where
    toString (I x) = "Int " +++ toString x
    toString (S xs) = "{" +++ convert xs +++ "}"

convert :: [Int] -> String
convert [] = ""
convert [x:xs] = toString x +++ " " +++ convert xs

Start :: *World -> *World
Start world = startEngine (simulate New [] "") world
