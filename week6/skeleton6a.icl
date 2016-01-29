module skeleton6a

/*******Done by Alexander Fedotov*******/
//The answer to the first question: we use the type (TaskState -> TaskResult Int) because in task1 = viewInformation "The answer is" 42 we return a curried function,
//which awaits for a state and returns a task result. So this type perfectly fits here.
//Regarding the second quection - without strictness (!*File) in the state I observe a heap full message when I try to evaluate the 4th task. I suppose that
//this caused by non-strict input handling.

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
:: *TaskResult a :== (a, TaskState)
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

viewInformation :: Description a TaskState -> TaskResult a | iTasksLite a
viewInformation d a s
    # console = s.console <<< d <<< ": " <<< print a <<< "\n"
    = (a, { console = console, store = s.store })

enterInformation :: Description TaskState -> TaskResult a | iTasksLite a
enterInformation d s
    # console = s.console <<< d <<< ": "
      (x, console) = freadline console
      y = (parse x)
    = case y of
      Nothing 
          # console = console <<< "Wrong format, try again.\n"
          = enterInformation d { console = console, store = s.store } 
      Just z = (z, {console = console, store = s.store})  

store :: a (StoreID a) TaskState -> TaskResult a | iTasksLite a
store a sid state
    # store = store_ a sid state.store
    = (a, { console = state.console, store = store})

retrieve :: (StoreID a) TaskState -> TaskResult a | iTasksLite a
retrieve sid state
    # x = retrieve_ sid state.store
    = (x, {console = state.console, store = state.store })

eval :: (TaskState -> TaskResult a) *File -> (a, *File) | iTasksLite a
eval taskFunc console
    # (r, st) = taskFunc ({store = newMap, console = console})
    = (r, st.console)

task0 :: TaskState -> TaskResult Int
task0 st = (42, st)

task1 :: (TaskState -> TaskResult Int)
task1 = viewInformation "The answer is" 42

task2 :: TaskState -> TaskResult Int
task2 st
    # (x, st) = enterInformation "Enter the answer" st
    =           viewInformation "The answer is" x st

task3 :: TaskState -> TaskResult Int
task3 st
    # (_, st) = store 1 intStore st
    =           retrieve intStore st
where
    intStore :: StoreID Int
    intStore = "intStore"
    
task3S :: TaskState -> TaskResult String
task3S st
    # (_, st) = store "Test" stringStore st
    =           retrieve stringStore st
where
    stringStore :: StoreID String
    stringStore = "stringStore"

task3Fail :: (TaskState -> TaskResult Int)
task3Fail = retrieve intStore
where
    intStore :: StoreID Int
    intStore = "intStore"
    
task3TypeFail :: TaskState -> TaskResult Int
task3TypeFail state 
    # (_, state) = store "Test" stringStore state 
    = retrieve intStore state
where 
    intStore :: StoreID Int
    intStore = "intStore"
    stringStore :: StoreID String
    stringStore = "intStore"

task4 :: TaskState -> TaskResult Void
task4 st
    # (_, st) = store [] ideaStore st
    = addIdea st
where
    addIdea st
        # (ideas, st) = retrieve ideaStore st
          (_,     st) = viewInformation "All ideas" ideas st
          (idea,  st) = enterInformation "Enter new idea" st
          (_,     st) = store (ideas ++ [toString (length ideas+1) +++ ". " +++ idea]) ideaStore st
        = addIdea st

    ideaStore :: StoreID [String]
    ideaStore = "ideas"

Start world
 #	(console, world) = stdio world
	console			 = console <<< "Welcome to iTasksLite\n\n"
    (r, console)     = eval task2 console
    console          = console <<< "\nThe result of the task is " <<< print r <<< ".\n"
	(_, world)	     = fclose console world
 = world

