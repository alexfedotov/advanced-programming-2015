module assignment5
/*******Done by Alex Fedotov (s4460952)*******/
import iTasks

derive class iTask Idea, ExtendedIdea

:: Idea = {
            idea    :: String,
            details :: Maybe Note
          }
:: ExtendedIdea = {
                    user      :: Name,
                    number    :: Int,
                    basicIdea :: Idea,
                    likes     :: [Name]
                  }
:: Name :== String

ideas :: Shared [ExtendedIdea]
ideas = sharedStore "Ideas" []

addNewIdea :: Name -> Task ExtendedIdea
addNewIdea name = forever (enterInformation ("You are logged in as " +++ name +++ " .Add an idea") [] >>* [OnAction (Action "Add" []) (hasValue (\i -> get ideas >>= \is -> set (indexate [{user = name, number = 0, basicIdea = i, likes = []}:is]) ideas >>| return {user = name, number = 1, basicIdea = i, likes = []}))])

indexate :: [ExtendedIdea] -> [ExtendedIdea]
indexate ideas = [{user = idea.user, number = i, basicIdea = idea.basicIdea, likes = idea.likes} \\ idea <- ideas & i <- [1..]]

askName :: Task Name
askName = enterInformation "Enter your name" []

existingIdeas :: Name -> Task ExtendedIdea
existingIdeas n = enterChoiceWithShared "" [] ideas >&^ viewSharedInformation "" [] >>* [
                                                                                         OnAction ActionOk (hasValue \i -> existingIdeas n),
                                                                                         OnAction (Action "Delete All" []) (always (set [] ideas >>| existingIdeas n)),
                                                                                         OnAction (Action "Delete" []) (hasValue (\i -> deleteIdea n i >>| existingIdeas n)),
                                                                                         OnAction (Action "Like" []) (hasValue (\i -> likeIdea n i >>| existingIdeas n))
                                                                                        ]
deleteIdea :: Name ExtendedIdea -> Task [ExtendedIdea]
deleteIdea n i = upd (\x -> indexate (filter (\y -> isValid n i y) x)) ideas
where
    isValid :: Name ExtendedIdea ExtendedIdea -> Bool
    isValid n i1 i2 = not (i2.number == i1.number && i2.user == n)
//It is possible to like and to unlike ideas    
likeIdea :: Name ExtendedIdea -> Task [ExtendedIdea]
likeIdea n i = upd (\x -> doLike n i x) ideas
where
    doLike :: Name ExtendedIdea [ExtendedIdea] -> [ExtendedIdea]
    doLike n i [] = []
    doLike n i [x:xs]
        | i.number == x.number
            | x.user <> n = if (isMember n x.likes) [{x&likes = filter ((<>) n) x.likes}:xs] [{x&likes = [n:x.likes]}:xs]
            | otherwise = [x:xs]
        | otherwise = [x:doLike n i xs]

Start :: *World -> *World
Start world = startEngine (askName >>= \n -> (addNewIdea n -||- existingIdeas n)) world