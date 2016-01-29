module skeleton6b

/*******Done by Alexander Fedotov*******/
//Some notes. This file contains only the solution to the second assignment. My try to implement trycatch for tasks you can find in the skeleton6b2.icl
//Here, everything works well, however task4 gives me a heap full message. I am stuck with finding the cause of it (everything seems correct)

import StdList, StdInt, StdChar, StdMisc, StdClass, StdString, StdFile, StdArray, Data.Maybe, Data.Map, Control.Monad, Data.Tuple, Data.Void
import qualified Text
from Text import class Text, instance Text String

class print a :: a -> String

instance print Void where print _ = "Void"
instance print String where print s = s
instance print Int where print i = toString i
instance print [a] | print a where print l = 'Text'.join ", " (map print l)

class parse a :: String -> Maybe a

instance parse Void where parse _ = Just Void

instance parse String where
    parse s = let len = size s
              in  Just (if (select s (len-1) == '\n') (s % (0, len - 2)) s) // remove newline
instance parse Int where
	parse s
		# len = size s
		| len > 0
			# s = if (select s (len-1) == '\n') (s % (0, len - 2)) s // remove newline
			# i = toInt s
			| toString i == s
				= Just i
			= Nothing

instance parse [a] | parse a where parse s = foldr (\xs list -> maybe Nothing (\e -> fmap (\l -> [e:l]) list) (parse xs)) (Just []) ('Text'.split "," s)

class iTasksLite a | print a & parse a & TC a

:: Description   :== String
:: StoreID a     :== String
:: Task a        = Task (*TaskState -> *(a, *TaskState))
:: *TaskState    = { console :: !*File
                   , store   :: Map String Dynamic
                   }

store_ :: a (StoreID a) (Map String Dynamic) -> Map String Dynamic | TC a
store_ v sid store = put sid (dynamic v) store

retrieve_ :: (StoreID a) (Map String Dynamic) -> a | TC a
retrieve_ sid store = case get sid store of
    Just (a :: a^) = a
    Just _         = abort "type error\n"
    Nothing        = abort "empty store\n"

instance Functor Task where
    fmap :: (a -> b) (Task a) -> Task b
    fmap f (Task a) = Task func
    where
        func st
            # (x, st) = a st
            = (f x, st)

instance Applicative Task where
    pure :: a -> Task a
    pure a = Task (\state -> (a, state))

    (<*>) infixl 4  :: (Task (a -> b)) (Task a) -> Task b
    (<*>) (Task a) (Task b) = Task func
    where
        func st
            # (x, st) = a st
              (y, st) = b st
            = (x y, st)

instance Monad Task where
    bind :: (Task a) (a -> Task b) -> Task b
    bind (Task f) g = Task func
    where
        func st
            # (x, st) = f st
              (Task h) = g x
            = h st 

eval :: (Task a) *File -> (a, *File) | iTasksLite a
eval (Task taskFunc) console
    # (r, {console}) = taskFunc {store = newMap, console = console}
    = (r, console)

task0 :: Task Int
task0 = return 42

viewInformation :: Description a -> Task a | iTasksLite a
viewInformation d a = Task (\s -> (a, { console = s.console <<< d <<< ": " <<< print a <<< "\n", store = s.store }))

enterInformation :: Description -> Task a | iTasksLite a
enterInformation d = Task (\s -> let (x, c) = freadline (s.console <<< d <<< ": ") in case (parse x) of
                                                                                       Nothing = let (Task f) = enterInformation d in f { console = c <<< "Wrong format, try again.\n", store = s.store }
                                                                                       Just y = (y, { console = c, store = s.store}))


store :: a (StoreID a) -> Task a | iTasksLite a
store a sid = Task (\s -> (a, { console = s.console, store = store_ a sid s.store}))


retrieve :: (StoreID a) -> Task a | iTasksLite a
retrieve sid = Task (\s -> let a = retrieve_ sid s.store in (a, {console = s.console, store = s.store }))




Task1 :: Task Int
Task1 = viewInformation "The answer is" 42

task2 :: Task Int
task2 =
     enterInformation "Enter the answer"
 >>= viewInformation "The answer is"

task3 :: Task Int
task3 =
        store 1 intStore
    >>| retrieve intStore
where
    intStore :: StoreID Int
    intStore = "intStore"

task3Fail :: Task Int
task3Fail = retrieve intStore
where
    intStore :: StoreID Int
    intStore = "intStore"

task4 :: Task Void
task4 =
        store [] ideaStore
    >>| addIdea
where
    addIdea =
                      retrieve ideaStore
        >>= \ideas -> viewInformation "All ideas" ideas
        >>|           enterInformation "Enter new idea"
        >>= \idea  -> store (ideas ++ [toString (length ideas+1) +++ ". " +++ idea]) ideaStore
        >>|           addIdea

    ideaStore :: StoreID [String]
    ideaStore = "ideas"

Start world
 #	(console, world) = stdio world
	console			 = console <<< "Welcome to iTasksLite" <<< "\n\n"
    (r, console)     = eval task4 console
    console          = console <<< "\n" <<< "The result of the task is " <<< print r <<< ".\n"
	(_, world)	     = fclose console world
 = world

