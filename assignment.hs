{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}

import Data.List
import Data.Ord
import Data.Function

data State = Pair Int Int | Triple Int Int Int
    deriving (Eq, Show)


data Test = TPair (Int, Int) (Int, Int) | TTrip (Int,Int,Int) (Int,Int,Int)
    deriving (Eq, Show)

valid :: State -> Test -> Bool
valid (Pair u g) (TPair (a, b) (c ,d)) =
    (a+b) == (c+d) &&
    (a+c) <= u &&
    (b+d) <= g
valid (Triple l h g) (TTrip (a, b, c) (d, e, f)) = 
    (a+b+c) == (d+e+f) &&
    (a+d) <= l &&
    (b+e) <= h &&
    (c+f) <= g

outcomes :: State -> Test -> [State]
outcomes (Pair u g) (TPair (a, b) (c, d))
    | valid (Pair u g) (TPair (a, b) (c, d)) == True = 
        [Pair unew gnew'] ++
        [Triple l h gnew] ++
        [Triple l h gnew]        
    | otherwise = error ("Invalid state or test")
        where
           unew = (u - (a + c))
           gnew = unew + g
           gnew' = g + a + c
           l   = a
           h   = c
outcomes (Triple l h g) (TTrip (a, b, c) (d, e, f))
    | valid (Triple l h g) (TTrip (a, b, c) (d, e, f)) == True = 
        [Triple lnew hnew gnew] ++
        [Triple lnew hnew gnew] ++
        [Triple lnew' hnew' gnew']            
    | otherwise = error ("Invalid state or test")
            where
                lnew = a+d
                lnew' = l-lnew
                hnew = b+e
                hnew' = h-hnew
                gnew = g+lnew'+hnew'
                gnew' = g+a+b+d+e

weighings :: State -> [Test]
weighings (Pair u g) = [TPair (a,b) (a+b, 0) | a<-[0..u], b<-[0..g],
     (a+b) > 0,
     ((2*a)+b) <= u,
     b <= g]
weighings (Triple l h g) = [TTrip (a, b, c) (d, e, f) | k1<-[1..k],
     (a, b, c) <- choices k1 (l, h, g),
     (d, e, f) <- choices k1 (l, h, g),
     c == 0 || f == 0,
     (a,b,c) <= (d,e,f),
     (c+f) <= g, (b+e) <= h, (a+d) <= l,
     (a+b+c) == (d+e+f),
     (a+b+c) > 0]
        where
            k = (l+h+g) `div` 2

choices :: Int -> (Int, Int, Int) -> [(Int, Int, Int)]
choices k (l, h, g) = [(i,j,e)| i<-[0..l], j<-[0..h], 
                            let e = k-i-j, e <= g, e >= 0]        

instance Ord State where
    (Pair _ _) < (Triple _ _ _) = False
    (Pair _ g1) < (Pair _ g2) = g2 < g1 
    (Triple _ _ g1) < (Triple _ _ g2) = g2 < g1

    (Pair _ _) <= (Triple _ _ _) = False
    (Pair _ g1) <= (Pair _ g2) = g2 <= g1
    (Triple _ _ g1) <= (Triple _ _ g2) = g2 <= g1

productive :: State -> Test -> Bool
productive s t = all (s > ) (outcomes s t)

tests :: State -> [Test]
tests s = filter (productive s) (weighings s)

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

height :: Tree -> Int
height (Stop s) = 0
height (Node _ xs) = 1 + maximum (map height xs)

minHeight :: [Tree] -> Tree
minHeight [] = error "Tree cannot be empty" 
minHeight xs = minimumBy (compare `on` height) xs

mktree :: State -> Tree
mktree s
    | (final s) == True = Stop s
    | otherwise = minHeight (map subTree (tests s))
        where
            subTree = (\t -> (Node t (map mktree (outcomes s t))))

data TreeH = StopH State | NodeH Int Test [TreeH]
    deriving Show

heightH :: TreeH -> Int
heightH (StopH s) = 0
heightH (NodeH h t ts) = h

treeH2tree :: TreeH -> Tree
treeH2tree (StopH s) = (Stop s)
treeH2tree (NodeH h t ths) = (Node t (map treeH2tree ths))

nodeH :: Test -> [TreeH] -> TreeH
nodeH t ths = NodeH h t ths
            where
                h = (+ 1) $ maximum (map heightH ths)
                -- we could use alternative 
                -- version ((+ 1) . maximum) (map heightH ths)

tree2treeH :: Tree -> TreeH
tree2treeH (Stop s) = (StopH s)
tree2treeH (Node t ts) = nodeH t (map tree2treeH ts)

mktreeH :: State -> TreeH
mktreeH s
    | (final s) == True = (StopH s)
    | otherwise = minimumBy (compare `on` heightH) $ map subTree (tests s)
        where       
            subTree = (\t -> nodeH t (map mktreeH (outcomes s t)))

optimal :: State -> Test -> Bool
optimal (Pair u g) (TPair (a,b) (ab,0)) = 
        (2 * a + b <= p) && (u - 2 * a - b <= q)
            where
                p = 3 ^ (t - 1)
                q = (p - 1) `div` 2
                t = ceiling (logBase 3 (fromIntegral (2 * u + k)))
                k = if g == 0 then 2 else 1
optimal (Triple l h g) (TTrip (a,b,c) (d,e,f)) = 
        (a+e) `max` (b+d) `max` (l-a-d+h-b-e) <= p
            where
                p = 3 ^ (t - 1)
                t = ceiling (logBase 3 (fromIntegral (l+h)))

bestTests :: State -> [Test]
bestTests s = filter (optimal s) (weighings s)

mktreeG :: State -> TreeH
mktreeG s
    | (final s) == True = StopH s
    | otherwise = (\t -> nodeH t (map mktreeG (outcomes s t))) bestTest
        where            
            bestTest = head (bestTests s)

mktreesG :: State -> [TreeH]
mktreesG s
    | (final s) == True = [StopH s]
    | otherwise = makeTree
        where
            optimalTests = bestTests s
            makeTree = map (\t -> nodeH t (map mktreeG (outcomes s t))) 
                $ optimalTests