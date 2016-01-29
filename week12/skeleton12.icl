module skeleton12

import Data.Maybe
import Control.Monad
import StdInt, StdString, StdBool
from StdFunc import id

/*

 Done by Alexander Fedotov

*/

/*
class arith x where
	lit :: a -> x a | toString a
	(+.) infixl 6 :: (x a) (x a) -> x a | + a // integer addition, Boolean OR
	(*.) infixl 7 :: (x a) (x a) -> x a | * a // integer multiplication, Boolean AND
class store x where
	read  :: (x Int)
	write :: (x Int) -> x Int
class truth x where
	(XOR) infixr 3 :: (x Bool) (x Bool) -> x Bool
	-.    :: (x Bool) -> x Bool
class (=.=) infix 4 x :: (x a) (x a) -> x Bool | == a
class except x where
	throw :: (x a)
	try   :: (x a) (x a) -> x a

class aexpr x | arith, store, except, =.= x
class bexpr x | arith, truth, except, =.= x
class expr x  | aexpr, bexpr x

:: Step a = Step (State -> (Maybe a, State))
:: State :== Int

instance Functor Step where
    fmap :: (a -> b) (Step a) -> (Step b)
    fmap f (Step g) = Step func
    where
        func state = case g state of
                     (Just x, newstate) = (Just (f x), newstate)
                     (Nothing, newstate) = (Nothing, newstate)

instance Applicative Step where
    pure :: a -> Step a
    pure x = Step (\s -> (Just x, s))
    (<*>) infixl 4 :: (Step (a -> b)) (Step a) -> Step b
    (<*>) (Step f) (Step g) = Step func
    where
        func state = case f state of
                     (Nothing, newstate) = (Nothing, newstate)
                     (Just x, newstate) = case g newstate of
                                          (Just y, brandnewstate) = (Just (x y), brandnewstate)
                                          (Nothing, brandnewstate) = (Nothing, brandnewstate)

instance Monad Step where
    bind :: (Step a) (a -> Step b) -> Step b
    bind (Step f) g = Step func
    where
        func state = case f state of
                     (Nothing, newstate) = (Nothing, newstate)
                     (Just x, newstate)
                         # (Step h) = g x
                         = h newstate

instance arith Step where
    lit i = return i
    (+.) x y = x >>= \a -> y >>= \b -> return (a + b)
    (*.) x y = x >>= \a -> y >>= \b -> return (a * b)

instance store Step where
    read = Step \s -> (Just s, s)
    write (Step x) = Step \s -> case (x s) of
                                (Just a, newstate) = (Just a, a)
                                other = other

instance truth Step where
    (XOR) x y = x >>= \a -> y >>= \b -> return (a && not b || not a && b)
    -. x = x >>= \a -> return (not a)

instance =.= Step where
    (=.=) x y = x >>= \a -> y >>= \b -> return (a == b)

instance except Step where
    throw = Step \s -> (Nothing, s)
    try (Step x) (Step y) = Step \s -> case x s of
                                       (Nothing, newstate) = y s
                                       other = other

eval :: (Step a) -> (Maybe a, State)
eval (Step x) = x 0

:: Show a = Show ([String] -> [String])

instance arith Show where
    lit i = Show \s -> [toString i: s]
    (+.) x y = let (Show a) = x
                   (Show b) = y in Show \s -> ["(": a [" + ": b [")": s]]]
    (*.) x y = let (Show a) = x
                   (Show b) = y in Show \s -> ["(": a [" * ": b [")": s]]]

instance store Show where
    read = Show \s -> ["read": s]
    write x = let (Show a) = x in Show \s -> ["(write ": a [")": s]]

instance truth Show where
    (XOR) x y = let (Show a) = x
                    (Show b) = y in Show \s -> ["(": a [" XOR ": b [")": s]]]
    -. x = let (Show a) = x in Show \s -> ["(not": a [")": s]]
instance =.= Show where
    (=.=) x y = let (Show a) = x
                    (Show b) = y in Show \s -> ["(": a [" == ": b [")": s]]]

instance except Show where
    throw = Show \s -> ["throw": s]
    try x y = let (Show a) = x
                  (Show b) = y in Show \s -> ["(try": a (b [")":s])]

seven :: e Int | aexpr e
seven = lit 3 +. lit 4

throw1 :: e Int | expr e
throw1 = lit 3 +. throw

six :: e Int | expr e
six = write (lit 3) +. read

try1 :: e Int | expr e
try1 = try throw1 (lit 42)

loge :: e Bool | expr e
loge = lit True *. -. (lit True)

comp :: e Bool | expr e
comp = lit 1 =.= lit 2 XOR -. (-. (lit True))

//wrong :: e Int | aexpr e
//wrong = lit 3 +. lit True
*/

instance + Bool where
    (+) x y = x || y

instance * Bool where
    (*) x y = x && y

:: Expr a
  = Lit a & toString a
  | (+.) infixl 6 (Expr a) (Expr a) & + a
  | (*.) infixl 7 (Expr a) (Expr a) & * a
  | Read (BM a Int)
  | Write (BM a Int) (Expr Int)
  | XOR (BM a Bool) (Expr Bool) (Expr Bool)
  | Not (BM a Bool) (Expr Bool)
  | E.b: Eq (BM a Bool) (Expr b) (Expr b) & == b
  | Throw
  | Try (Expr a) (Expr a)

:: BM a b = { t :: a -> b, f :: b -> a }
bm :: BM a a
bm = { f = id, t = id }

:: State :== Int

class show a :: a [String] -> [String]

instance show (Expr a) where
    show (Lit x) s = [toString x: s]
    show (x +. y) s = ["(": show x [" + ": show y [")": s]]]
    show (x *. y) s = ["(": show x [" * ": show y [")": s]]]
    show (Read bm) s = ["read":s]
    show (Write bm x) s = ["(": show x [")": s]]
    show (XOR bm x y) s = ["(": show x [" XOR ": show y [")": s]]]
    show (Not bm x) s = ["(Not": show x [")": s]]
    show (Eq bm x y) s = ["(": show x [" == ": show y [")": s]]]
    show Throw s = ["throw":s]
    show (Try x y) s = ["(Try": show x (show y [")": s])]

eval :: (Expr a) State -> (Maybe a, State)
eval (Lit x) s = (Just x, s)
eval (x +. y) s = let (a, newstate) = (eval x s) in case a of
                                                    Nothing = (Nothing, newstate)
                                                    (Just aa) = let (b, brandnewstate) = (eval y newstate) in case b of
                                                                                                              Nothing = (Nothing, brandnewstate)
                                                                                                              (Just bb) = (Just (aa + bb), brandnewstate)
eval (x *. y) s = let (a, newstate) = (eval x s) in case a of
                                                    Nothing = (Nothing, newstate)
                                                    (Just aa) = let (b, brandnewstate) = (eval y newstate) in case b of
                                                                                                              Nothing = (Nothing, brandnewstate)
                                                                                                              (Just bb) = (Just (aa * bb), brandnewstate)
eval (Read bm) s = (Just (bm.f s), s)
eval (Write bm x) s = let (a, newstate) = (eval x s) in case a of
                                                        Nothing = (Nothing, newstate)
                                                        (Just aa) = (Just (bm.f aa), aa)
eval (XOR bm x y) s = let (a, newstate) = (eval x s) in case a of
                                                        Nothing = (Nothing, newstate)
                                                        (Just aa) = let (b, brandnewstate) = (eval y newstate) in case b of
                                                                                                                  Nothing = (Nothing, brandnewstate)
                                                                                                                  (Just bb) = (Just (bm.f (aa && not bb || not aa && bb)), brandnewstate)
eval (Not bm x) s = let (a, newstate) = (eval x s) in case a of
                                                      Nothing = (Nothing, newstate)
                                                      (Just aa) = (Just (bm.f (not aa)), newstate)
eval (Eq bm x y) s = let (a, newstate) = (eval x s) in case a of
                                                    Nothing = (Nothing, newstate)
                                                    (Just aa) = let (b, brandnewstate) = (eval y newstate) in case b of
                                                                                                              Nothing = (Nothing, brandnewstate)
                                                                                                              (Just bb) = (Just (bm.f (aa == bb)), brandnewstate)
eval (Throw) s = (Nothing, s)
eval (Try x y) s = let (a, newstate) = (eval x s) in case a of
                                                     Nothing = eval y s
                                                     (Just aa) = (Just aa, newstate)

Start = toString True
