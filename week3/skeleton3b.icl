module skeleton3b

/*
	Advanced Programming.
	Skeleton for exercise 3.3 and 3.4.
	To be used in a project with the environment Everything, 
	or StdEnv with an import of StdMaybe from StdLib

	Pieter Koopman, pieter@cs.ru.nl
*/

/*******Done by Alexander Fedotov*******/

import StdEnv, StdGeneric, StdMaybe, GenEq

//------------------ show --------------
generic show_ a :: a [String] -> [String]

show_{|Int|}  i c = [toString i:c]
show_{|Bool|} b c = [toString b:c]

show a = show_{|*|} a []

//------------------ parse --------------

:: Result a :== Maybe (a, [String])

generic parse a :: [String] -> Result a

parse{|Bool|} ["True" :r] = Just (True ,r)
parse{|Bool|} ["False":r] = Just (False,r)
parse{|Bool|} _ = Nothing

//------------------ some data types --------------

:: T		= C
:: Color	= Red | Yellow | Blue
:: Tree a	= Tip | Bin a (Tree a) (Tree a)

//------------------ general useful --------------

instance + String where (+) s t = s+++t
derive bimap Maybe, []

//------------------ to test if parse and show work properly --------------

test :: t -> Bool | gEq{|*|}, show_{|*|}, parse{|*|} t
test x
	= case parse{|*|} (show x) of
		Just (y,[])	= x === y
		_			= False

/**************** End Prelude, add all new code below this line *************************/
show_{|UNIT|} _ c = c
show_{|CONS of {gcd_name, gcd_arity}|} f (CONS x) c = if (gcd_arity > 0) ["(":gcd_name: f x (c ++ [")"])] ([gcd_name: f x c])
show_{|PAIR|} f h (PAIR x y) c = f x (h y c) 
show_{|EITHER|} f h (LEFT x) c = f x c
show_{|EITHER|} f h (RIGHT y) c = h y c
show_{|OBJECT|} f (OBJECT x) c = f x c
show_{|FIELD|} f (FIELD x) c = f x c

parse{|Int|} x = Just (toInt (hd x), tl x)
parse{|UNIT|} x = Just (UNIT, x)
parse{|CONS of {gcd_name, gcd_arity}|} f ["(":n:x] = if (n == gcd_name) (case f x of Just (xx, r) = Just (CONS xx, init r);otherwise = Nothing) (Nothing)
parse{|CONS of {gcd_name, gcd_arity}|} f [n:x] = if (n == gcd_name) (case f x of Just (xx, r) = Just (CONS xx, r);otherwise = Nothing) (Nothing)
parse{|PAIR|} f h x = case f x of
                      Just (xx, rr) = case (h rr) of
                                      Just (y,rrr) = Just (PAIR xx y, rrr)
                                      otherwise = Nothing
                      otherwise = Nothing
parse{|EITHER|} f h x = case f x of
                        Just (xx, r) = Just (LEFT xx, r)
                        otherwise = case h x of
                                    Just (y,r) = Just (RIGHT y,r)
                                    otherwise = Nothing
parse{|OBJECT|} f x = case f x of
                      Just (xx, r) = Just (OBJECT xx, r)
                      otherwise = Nothing
parse{|FIELD|} f x = case f x of
                     Just (xx, r) = Just (FIELD xx, r)
                     otherwise = Nothing

derive show_ T, Color, [], Tree, (,)
derive parse T, Color, [], Tree, (,)
derive gEq Color
//------------------ tests --------------
Start 
 =	[ and [ test i \\ i <- [-25 .. 25]]
	, and [ test c \\ c <- [Red,Yellow,Blue]]
	, test [1 .. 3]
	, test [(a,b) \\ a <- [1 .. 2], b <- [5 .. 7]]
	]
