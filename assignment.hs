import Data.List
import Data.Ord
import Data.Function

{-
Questions
1. Why we use data type, what are benefits 
    of using such a feature of that language ?
2. Benetif of representing problem in such a way ?
-}

{-Ideas, check if State and Test are functors, probably not-}

{- Pair (u,g) - unknown, geniue-}
{- Triple (l,h,g)- lighter, heavier, geniue -}
data State = Pair Int Int | Triple Int Int Int
    deriving (Eq, Show)

{-TPair (a,b) (c,d) - denotes, weighting a coins from file U plus b coins from pile G 
against c coins from pile U, d coind from pile G-}
data Test = TPair (Int, Int) (Int, Int) | TTrip (Int,Int,Int) (Int,Int,Int)
    deriving (Eq, Show)

{-
Determine whether a given test is valid in a given state, according to the
above criteria. For example valid (Pair 12 0) (TPair (3,0) (3,0)) = True
-}
-- todo check Eq class type, and maybe reuse it ?? Maybe introduce new typeclass ?
-- valid (Pair 12 0) (TPair (3,0) (3,0))

valid :: State -> Test -> Bool
valid (Pair u g) (TPair (a, b) (c ,d)) = (u+g) > (a+b+c+d)
valid (Triple l h g) (TTrip (a, b, c) (d, e, f)) = (l+h+g) > (a+b+c+d+e+f)

{-Test data:
    As stated in instruction (page 2). TPair can be only conducted in a Pair state, and
    a TTrip test in a Triple state.

    - outcomes (Pair 12 0) (TPair (3,0) (3,0))
        - expected [Pair 6 6,Triple 3 3 6,Triple 3 3 6]
    - outcomes (Pair 4 8) (TPair (1,0) (1,0))
        - expected [Pair 2 10,Triple 1 1 10,Triple 1 1 10]
    
    Criterion under which outomes make sense for us.
        - Ignore tests of the form TPair ... in which both b,d > 0
        - Assuming b >= d, then it suffices to consider TPair (a, b - d) (c, 0)
        - Both applies to TTrip accordingly

        - Scales are symmetric, (TPair, TTrip)
        - Test must guaranteed to increase our knowledge
-}
outcomes :: State -> Test -> [State]
outcomes (Pair u g) (TPair (a, b) (c, d)) 
    =   [Pair gcc gc] ++ [Triple l h gcc] ++ [Triple l h gcc]
            where
                gcc = u - (a + c)  
                gc  = g + a + c
                l   = a
                h   = c
outcomes (Triple l h g) (TTrip (a, b, c) (d, e, f)) 
    =   [Triple (l-a) h (g+d)]
        ++ [Triple (l-a-b-d-e) (h+a+b) (g+d+e)]
        ++ [Triple (l-a) h (g+d)]  

{-
expected:
 - weighings (Pair 3 3) = [TPair (0, 1) (1, 0), TPair (0, 2) (2, 0),
 TPair (0, 3) (3, 0), TPair (1, 0) (1, 0), TPair (1, 1) (2, 0)]
-}                
weighings :: State -> [Test]
weighings (Pair u g) = [TPair (a,b) (a+b, 0) | a<-[0..u], b<-[0..g],
     (a+b) > 0,
     ((2*a)+b) <= u,
     b <= g]
weighings (Triple l h g) = [TTrip (a, b, c) (d, e, f)| k1<-[1..k],
     (a, b, c) <- choices k1 (l, h, g),
     (d, e, f) <- choices k1 (l, h, g),
      c == 0 || f == 0, (a,b,c) <= (d,e,f), (c+f) <= g, (b+e) <= h, (a+d) <= l, (a+b+c) == (d+e+f), (a+b+c) > 0]
        where
            k = (l+h+g) `div` 2

{-
expected:
- choices 3 (2,2,2) = 
-}
choices :: Int -> (Int, Int, Int) -> [(Int, Int, Int)]
choices k (l, h, g) = [(i,j,k-i-j)| i<-[0..l], j<-[0..h], (k-i-j) <= g, (k-i-j) >= 0]

{-Need to be checked-}
instance Ord State where
    (Pair _ _) < (Triple _ _ _) = False
    (Pair _ g1) < (Pair _ g2) = g2 < g1 
    (Triple _ _ g1) < (Triple _ _ g2) = g2 < g1

    (Pair _ _) <= (Triple _ _ _) = False
    (Pair _ g1) <= (Pair _ g2) = g2 <= g1
    (Triple _ _ g1) <= (Triple _ _ g2) = g2 <= g1

productive :: State -> Test -> Bool
productive s t = s > (head $ sort $ outcomes s t)

{-For example state Triple 3 0 6 admits 5 weightings, 
but one of these is unproductive, 
so tests returns only 4 productive weightings-}

tests :: State -> [Test]
tests s = filter (productive s) (weighings s)

{--4. Decision Tree-}

data Tree = Stop State | Node Test [Tree]
    deriving (Show)

final :: State -> Bool
final (Pair u g)
    | u == 0 = True
    | otherwise = False
final (Triple l h g)
    | l == 1 && h == 0 = True
    | l == 0 && h == 1 = True
    | otherwise = False 

{-
Example data : height (Node (TPair (3,0) (3,0)) [(Stop (Pair 6 6))])
-}

testTree :: Tree
testTree = (Node (TPair (6,0) (6,0)) [Node (TPair (3,0) (3,0)) [], Node (TTrip (1,0,0) (1,0,0)) [], Node (TTrip (1,0,0) (1,0,0)) []])

nestedTree :: Tree
nestedTree = (Node (TPair (6,0) (6,0)) 
    [Node (TPair (3,0) (3,0)) [testTree], Node (TTrip (1,0,0) (1,0,0)) [testTree], Node (TTrip (1,0,0) (1,0,0)) [testTree]])

{-use maximum, and foldable, needs optimisation and generalization-}

height :: Tree -> Int
height (Stop s) = 0
height (Node _ []) = 0 
height (Node _ (x:xs)) = 1 + height x
height (Node _ (x:y:xs)) = 1 + max (height x) (height y)
height (Node _ (x:y:z:xs)) = (+) 1 $ max (height z) $ max (height x) (height y)

{-Check if I could instance Ord typeclass for Tree,
 such that t < t' iff height t < height t' -}

-- instance Ord Tree where
--     t < t' = height t < height t'
--     t <= t' = height t <= height t'
{--def need optimisation, use foldl ?-}
minHeight :: [Tree] -> Tree
minHeight xs = snd $ head $ sortBy (compare `on` (\(y,_) -> y)) $ map (\x -> (height x, x)) xs

{-intermediate setps-}
-- minHeight' :: [Tree] -> [(Int, Tree)]
-- minHeight' xs = sortBy (compare `on` (\(y,_) -> y)) $ map (\x -> (height x, x)) xs

mktree :: State -> Tree
mktree s
    | (final s) == True = Stop s
    | otherwise = minHeight $ map (\(t, ss) -> (Node t [])) $ map ((\t -> (t, outcomes s))) $ tests s

--lazy eval on Test->State

mktree' :: State -> [Tree]
mktree' s
    | (final s) == True = [Stop s]
    | otherwise = makeTree $ productiveOutcomes $ productiveTests
        where
            productiveTests = tests s
            productiveOutcomes = map ((\t -> (t, outcomes s t)))
            makeTree = map (\(t, xs) -> (Node t (concat $ map mktree' xs))) {-- check that!!!-}

{-5 Caching heights-}

data TreeH = StopH State | NodeH Int Test [TreeH]
    deriving Show

heightH :: TreeH -> Int
heightH (StopH s) = 0
heightH (NodeH h t ts) = h

{- Convert labelled TreeH back to the corresponding Tree -}
treeH2tree :: TreeH -> Tree
treeH2tree (StopH s) = (Stop s)
treeH2tree (NodeH h t []) = (Node t [])
treeH2tree (NodeH h t [th]) = (Node t [treeH2tree th]) -- dont need this
treeH2tree (NodeH h t ths) = (Node t (map treeH2tree ths))

--nodeH :: Test -> [TreeH] -> TreeH

tree2treeH :: Tree -> TreeH
tree2treeH (Stop s) = (StopH s)
tree2treeH (Node t []) = (NodeH 0 t [])
tree2treeH (Node t [ts]) = (NodeH 0 t [tree2treeH ts])
tree2treeH (Node t ts) = (NodeH 0 t (map tree2treeH ts))

-- mktreeH :: State -> TreeH
-- mktreeH s
--     | (final s) == True = (StopH s)
--     | otherwise = makeTree $ productiveOutcomes $ productiveTests
--         where
--             productiveTests = tests s
--             productiveOutcomes = map ((\t -> (t, outcomes s t)))
--             makeTree = map (\(t, xs) -> (NodeH 0 t (concat $ map mktree' xs))) {-- check that!!!-}

{-use :set +s to check eval time-}

{-6. Greedy solution-}

optimal :: State -> Test -> Bool
optimal (Pair u g) (TPair (a,b) (ab,0)) = (2 * a + b <= p) && (u - 2 * a - b <= q)
        where
            p = 3
            q = (p - 1) `div` 2
            t = ceiling (logBase 3 (fromIntegral (2 * u + k)))
            k = if g == 0 then 2 else 1
optimal (Triple l h g) (TTrip (a,b,c) (d,e,f)) = (a+e) `max` (b+d) `max` (1-a-d+h-b-e) <= p
        where
            p = 3
            t = ceiling (logBase 3 (fromIntegral (l+h)))

-- bestTests :: State -> [Test]

-- mktreeG :: State -> TreeH

-- mktreesG :: State -> [TreeH]

-- other questions
-- use modules ??