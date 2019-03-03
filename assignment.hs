import Data.List
import Data.Ord

{-
Questions
1. Why we use data type, what are benefits 
    of using such a feature of that language ?
2. Benetif of representing problem in such a way ?
-}

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
-- todo check Eq class type, and maybe reuse it ??
-- valid (Pair 12 0) (TPair (3,0) (3,0))

valid :: State -> Test -> Bool
valid (Pair p1 p2) (TPair (x1, x2) (x3,x4)) = (p1+p2) > (x1+x2+x3+x4)
valid (Triple p1 p2 p3) (TTrip (x1, x2, x3) (x4, x5, x6)) = (p1+p2+p3) > (x1+x2+x3+x4+x5+x6)

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
outcomes (Pair p1 p2) (TPair (x1, x2) (x3, x4)) 
    =   [Pair unknownCountLeft geniueCount]
         ++ [Triple lighterCount heavierCount unknownCount]
         ++ [Triple heavierCount lighterCount unknownCount]
        where
            unknownCount        = (p1 + p2) - (x1 + x3)
            unknownCountLeft    = p1 - (x1 + x3)  
            geniueCount         = p2 + x1 + x3
            lighterCount        = x1
            heavierCount        = x3
outcomes (Triple p1 p2 p3) (TTrip (x1, x2, x3) (x4, x5, x6)) = []
        where

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
weighings (Triple l h g) = [TTrip (a, a, a) (a, a, a)| a<-[0..1]]

{-
expected:
- choices 3 (2,2,2) = 
-}
choices :: Int -> (Int, Int, Int) -> [(Int, Int, Int)]
choices k (l, h, g) = [(i,j,k-i-j)| i<-[0..l], j<-[0..h], (k-i-j) <= g, (k-i-j) >= 0]

-- instance Ord State where ...

-- productive :: State -> Test -> Bool

{-For example state Triple 3 0 6 admits 5 weightings, 
but one of these is unproductive, 
so tests returns only 4 productive weightings-}

-- tests :: State -> [Test]

{--4. Decision Tree-}

data Tree = Stop State | Node Test [Tree]
    deriving Show

-- final :: State -> Bool

-- height :: Tree -> Int

-- minHeight :: [Tree] -> Tree

-- mktree :: State -> Tree

{-5 Caching heights-}

data TreeH = StopH State | NodeH Int Test [TreeH]
    deriving Show

-- heightH :: TreeH -> Int
-- heightH (StopH s) = 0
-- heightH (NodeH h t ts) = h

{- Convert labelled TreeH back to the corresponding Tree -}
-- treeH2tree :: TreeH -> Tree

-- nodeH :: Test -> [TreeH] -> TreeH

-- tree2treeH :: Tree -> TreeH

-- mktreeH :: State -> TreeH

{-use :set +s to check eval time-}

{-6. Greedy solution-}

-- optimal :: State -> Test -> Bool

-- bestTests :: State -> [Test]

-- mktreeG :: State -> TreeH

-- mktreesG :: State -> [TreeH]

-- other questions
-- use modules ??