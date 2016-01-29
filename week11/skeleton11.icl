module skeleton11

/*

Done by Alexander Fedotov

Remarks.
I admit that the computation of possible results could be done in a more concise way (e.g. by generation of sequences of computation
using list comprehentions). However, I found interesting to have a tree of computation sequences.
As for advantages of specifying a semantics in a functional language, the big advantage is that by using a widely used in academia language as Haskell, one gets concise and standartized notation whereas ordinary mathematical notation may be vague in some cases and a bit less standartized (there are several notations possible).
As a downside of expressing semantics in a functional language I can mention that this way is not accepted by everyone in academia, some prefer ordinary mathematical notation.
An imaginary  case in which I may not want to specify semantics in a functional language is when I write a paper on various functional languages, using one particular language to express semantics is not the best and can be confusing. Ordinary mathematical notation (abstract from any particular function language) may be more preferable.

*/

import StdEnv

:: Store :== Int

initialStore :: Int
initialStore = 0

:: Prog :== [Instr]

:: Instr = Write Expr | Atomic [Instr]

:: Expr = Int   Int
        | Plus  Expr Expr
        | Times Expr Expr
        | Read

evalExpr :: Expr -> (Store -> Int)
evalExpr (Int i) = \store -> i
evalExpr (Plus e1 e2) = \store -> let x = evalExpr e1 store
                                      y = evalExpr e2 store
                                  in x + y
evalExpr (Times e1 e2) = \store -> let x = evalExpr e1 store
                                       y = evalExpr e2 store
                                   in x * y
evalExpr Read = \store -> store

rewriteProg :: [Instr] -> (Store -> (Int, [Instr]))
rewriteProg [] = \store -> (store, [])
rewriteProg [(Write e):xs] = \store -> ((evalExpr e store), xs)
rewriteProg [(Atomic instr):xs] = rewriteAtomic instr
where
    rewriteAtomic :: [Instr] -> (Store -> (Int, [Instr]))
    rewriteAtomic [] = \store -> (store, [])
    rewriteAtomic [(Write e):xs] = \store -> rewriteAtomic xs (evalExpr e store)

rewriteTree :: (Tree ([Instr], [[Instr]])) -> [Int]
rewriteTree Tip = []
rewriteTree (Bin a []) = []
rewriteTree (Bin a xs) = flatten [(rewriteTree` 0 x) \\ x <- xs]
where
    rewriteTree` :: Int (Tree ([Instr], [[Instr]])) -> [Int]
    rewriteTree` store (Bin a [Tip]) = [(fst (rewriteProg (fst a) store))]
    rewriteTree` store (Bin a xs) = flatten [(rewriteTree` (fst (rewriteProg (fst a) store)) x) \\ x <- xs]

possibleResults :: [Prog] -> [Int]
possibleResults progs = rewriteTree (executionTree progs)

:: Tree a = Tip | Bin a [Tree a]

executionTree :: [[Instr]] -> Tree ([Instr], [[Instr]])
executionTree instr = makeTree [] instr
where
    makeTree :: [Instr] [[Instr]] -> Tree ([Instr],[[Instr]])
    makeTree a [] = Tip
    makeTree a [x] = Bin (a,[x]) [(Bin ([hd x], [tl x]) [Bin (tl x, []) [Tip]])]
    makeTree a xs = Bin (a,xs) [makeTree (hdIndex i xs) (tlIndex i xs) \\ i <- [1 .. length xs]]

tlIndex :: Int [[Instr]] -> [[Instr]]
tlIndex 0 xs = xs
tlIndex i [x:xs]
    | i > length [x:xs] = [x:xs]
    | i > 1 = [x: tlIndex (i-1) xs]
    | i == 1 = filter (\a -> length a <> 0) [tl x:xs]

hdIndex :: Int [[Instr]] -> [Instr]
hdIndex 0 xs = []
hdIndex i [x:xs]
    | i > length [x:xs] = []
    | i > 1 = hdIndex (i-1) xs
    | i == 1 = [hd x]

prog0 = [ Write (Int 12)
        , Write (Plus Read (Int 1))
        ]
prog1 = [ Write (Times Read (Int 2)) ]
prog2 = [ Atomic [Write (Int 12), Write (Plus Read (Int 1))]]
test0 = [ prog0 ]       // result should be [13]
test1 = [ prog0, prog1] // result should be [13, 25, 26] (or any permutation, the order doesn't matter)
test2 = [ prog2, prog1] // extra test which shows the difference between single instructions as atoms and the presense of atomic set of instructions

Start = possibleResults test1
