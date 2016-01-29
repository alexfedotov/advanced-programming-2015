module assignment10

import StdEnv, StdGeneric, GenEq, genLibTest

//derive genShow Matcher //Needed for the Deep version!


/*

Done by Alexander Fedotov

Result of the test run:

["
***Test run is finished***","
Total: ","9","
Passed: ","4","
Failed: ","5","
*********Details**********: ","
Error in test: ","Red, yellow and blue",", expected result: ",""Who is afraid of red, yellow and blue"","ContainsString","Who is afraid of red, yellow and blue","(does not hold)
","
Error in test: ","[0..3] (Either (EqualTo [1]) (Contains 7))",", expected result: ","[0,1,2,3]","Either","EqualTo","[0,1,2,3]","Contains","[0,1,2,3]","(does not hold)
","
Error in test: ","[0..3] (Contains 7)",", expected result: ","[0,1,2,3]","Contains","[0,1,2,3]","(does not hold)
","
Error in test: ","(length [0..3]) is not 4",", expected result: ","4","Not","EqualTo","4","(does not hold)
","
Error in test: ","(3*3) (EqualTo (3+3))",", expected result: ","9","EqualTo","9","(does not hold)
"]

*/


:: Test :== TestState -> TestState
:: TestState = { pass :: Int, fail :: Int, report :: [String] }

runTests :: Test -> [String]
runTests f = printStats (f { pass = 0, fail = 0, report = []})

printStats :: TestState -> [String]
printStats s = ["\n***Test run is finished***", "\nTotal: ", toString (s.pass + s.fail), "\nPassed: ", toString s.pass, "\nFailed: ", toString s.fail,
                "\n*********Details**********: ": s.report]

instance * Test where
    (*) t1 t2 = \state -> t2 (t1 state)

//Deep version
/*
:: Matcher a =
      Is (Matcher a)
    | EqualTo a
    | LessThan a
    | Not (Matcher a)
    | Either (Matcher a) (Matcher a)

eval :: a (Matcher a) -> Bool | ==, < a
eval x (Is y) = eval x y
eval x (EqualTo y) = x == y
eval x (LessThan y) = x < y
eval x (Not y) = if (eval x y) False True
eval x (Either y z) = (eval x y) || (eval x z)

AssertThat :: String a (Matcher a) -> Test | ==, <, genShow{|*|} a
AssertThat descr x y = \state -> if (eval x y)
                                    ({ state & pass = state.pass+1 })
                                    ({ fail = state.fail + 1, pass = state.pass, report = ["\nError in test: ", descr,
                                                                                           ", expected result: ",
                                                                                           show1 x, show1 y, "(does not hold)\n": state.report]})

*/
//Shallow version

:: Matcher a :== a -> (Bool, [String])

Is :: (Matcher a) a -> (Bool, [String])
Is x y = (fst (x y), ["Is": snd (x y)])

EqualTo :: a a -> (Bool, [String]) | ==, genShow {|*|} a
EqualTo x y = (x == y, ["EqualTo", show1 y])

LessThan :: a a -> (Bool, [String]) | <, genShow {|*|} a
LessThan x y = (x < y, ["LessThan", show1 y])

Not :: (Matcher a) a -> (Bool, [String])
Not x y = (not (fst (x y)),["Not": snd (x y)])

Either :: (Matcher a) (Matcher a) a -> (Bool, [String])
Either x y z = (fst (x z) || fst (y z), ["Either": (snd (x z) ++ snd (y z))])

Contains :: a [a] -> (Bool, [String]) | Eq, genShow {|*|} a
Contains x xs = (isMember x xs, ["Contains", show1 xs])

ContainsString :: String String -> (Bool, [String])
ContainsString x y = (checkStrings x y, ["ContainsString", y])
where
    checkStrings :: String String -> Bool
    checkStrings s1 s2
        | (size s1 > size s2) = False
        | otherwise = if (s1 == s2 % (0, (size s1) - 1)) True (checkStrings s1 (s2 % (1, (size s2) - 1)))

AssertThat :: String a (Matcher a) -> Test | ==, <, genShow{|*|} a
AssertThat descr x y = \state -> if (fst (y x))
                                     ({ state & pass = state.pass+1 })
                                     ({ fail = state.fail + 1, pass = state.pass, report = ["\nError in test: ", descr,
                                                                                            ", expected result: ", show1 x:
                                                                                            (snd (y x)) ++ ["(does not hold)\n"] ++ state.report]})


a1 = AssertThat "(2*2) (Is (EqualTo (2+2)))" (2*2) (Is (EqualTo (2+2)))
a2 = AssertThat "(3*3) (EqualTo (3+3))" (3*3) (EqualTo (3+3))
a3 = AssertThat "(length [0..3]) is not 4" (length [0..3]) (Not (EqualTo 4))
a4 = AssertThat "[0..3] (Contains 2)" [0..3] (Contains 2)
a5 = AssertThat "[0..3] (Contains 7)" [0..3] (Contains 7)
a6 = AssertThat "[0..3] (Either (EqualTo [1]) (Contains 7))" [0..3] (Either (EqualTo [1]) (Contains 7))
a7 = AssertThat "’hello world’ (ContainsString ’hello’)" "hello world" (ContainsString "hello")
a8 = AssertThat "’hello world’ (ContainsString ’world’)" "hello world" (ContainsString "world")
a9 = AssertThat "Red, yellow and blue" "Who is afraid of red, yellow and blue" (ContainsString "Red")


Start = runTests (a1 * a2 * a3 * a4 * a5 * a6 * a7 * a8 * a9)
