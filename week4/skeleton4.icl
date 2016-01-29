module skeleton4

/*
	Advanced Programming.
	Skeleton for assignment 4.
	To be used in a project with the environment iTasks.
	Pieter Koopman, pieter@cs.ru.nl
*/

/*******Done by Alexander Fedotov*******/

import iTasks, Text

:: Name	:== String

:: Idea = { title :: String, descr :: Maybe Note }
:: ExtendedIdea = { author :: Name, nr :: Int, basic :: Idea }

:: NamedIdea = { name :: Name, idea :: [ExtendedIdea] }
derive class iTask NamedIdea, Idea, ExtendedIdea 

doIdentified :: (Name -> Task x) -> Task x | iTask x
doIdentified task
	=   enterInformation "Enter your name" []
	>>= task

editIdea :: Name -> Task [ExtendedIdea]
editIdea n
	=           enterInformation (n +++ " add your idea") []
	>>= \idea . return (extend n idea)

mainTask =   doIdentified editIdea
		 >>= viewInformation "Ideas" [ViewWith (filter (\n -> checkLength n))]


checkLength :: ExtendedIdea -> Bool
checkLength m = case m.basic.descr of
                Nothing = True
                (Just (Note s)) = if (textSize s <= 10) True False 
    
extend :: Name [Idea] -> [ExtendedIdea]
extend n xs = extend` n 1 xs
where		 
    extend` :: Name Int [Idea] -> [ExtendedIdea]
    extend` n i [] = []
    extend` n i [x:xs] = [{author = n, nr = i, basic = x}: extend` n (i+1) xs]

Start :: *World -> *World
Start world = startEngine mainTask world
