module Main where

import Math.Operad 
--import Math.Operad.PPrint (pp,pP)
--import Math.Operad.OrderedTree (TreeOrdering)
import Data.List (nub)
import Control.Concurrent
import Utils

a = corolla 1 [1,2]
b = corolla 2 [1,2]
-- | Using the path sequence, the leaf orders and order reversal, we can get 8 different orderings
-- from one paradigm. These are given by 'PathPerm', 'RPathPerm', 'PathRPerm', 'RPathRPerm' for the 
-- variations giving (possibly reversed) path sequence comparison precedence over (possibly reversed)
-- leaf permutations; additionally, there are 'PermPath', 'RPermPath', 'PermRPath' and 'RPermRPath'
-- for the variations with the opposite precedence.

ts :: [OperadElement Integer Rational RPermRPath]
ts = map oet
  [shuffleCompose 1 [1,2,3] a a, -- (ab)c   :1
   shuffleCompose 2 [1,2,3] a a,  -- a(bc)
   shuffleCompose 1 [1,3,2] a a,  -- (ac)b   :3
   shuffleCompose 2 [1,2,3] a b, -- a(cb)
   shuffleCompose 1 [1,2,3] a b, -- (ba)c   :5
   shuffleCompose 1 [1,3,2] b a, -- b(ac)
   shuffleCompose 2 [1,2,3] b a, -- (bc)a   :7
   shuffleCompose 1 [1,3,2] b b, -- b(ca)
   shuffleCompose 1 [1,3,2] a b, -- (ca)b   :9
   shuffleCompose 1 [1,2,3] b a, -- c(ab)
   shuffleCompose 2 [1,2,3] b b, -- (cb)a   :11
   shuffleCompose 1 [1,2,3] b b] -- c(ba)

opSum = foldr (+) zero   
-- identity of flexible algebras: (x,y,x) = 0
-- g1 = opSum $ zipWith (.*.) [1,-1] $ map ((ts!!) . (subtract 1)) [1,2]
g1 = opSum $ zipWith (.*.) [1,-1,1,-1] $ map ((ts!!) . (subtract 1)) [1,2,11,12]
g2    = opSum $ zipWith (.*.) [1,-1,1,-1] $ map ((ts!!) . (subtract 1)) [5,6,9,10]
g3    = opSum $ zipWith (.*.) [1,-1,1,-1] $ map ((ts!!) . (subtract 1)) [7,8,3,4]
-- identity for Lie admissible algebras: [[ab]c]+[[bc]a]+[[ca]b] = 0
glie  = opSum $ zipWith (.*.) [1,-1,-1,1,1,-1,-1,1,1,-1,-1,1] $ map ((ts!!) . (subtract 1)) [1,10,5,12,7,11,2,4,9,3,8,6]   
-- initial list of shuffle identities  	
ad0  = [g1,g2,g3,glie]

adn1 = stepOperadicBuchberger [] ad0
ad1 = nub $ ad0 ++ adn1
adn2 = stepOperadicBuchberger ad0 adn1
ad2 = nub $ ad1 ++ adn2
adn3 = stepOperadicBuchberger ad1 adn2
ad3 = nub $ ad2 ++ adn3

--adn1 = stepInitialOperadicBuchberger 3 [] ad0
--ad1 = nub $ ad0 ++ adn1
--adn2 = stepInitialOperadicBuchberger 4 ad0 adn1
--ad2 = nub $ ad1 ++ adn2
--adn3 = stepInitialOperadicBuchberger 5 ad1 adn2
--ad3 = nub $ ad2 ++ adn3

second_us = 1000000
main = do
  threadID <- forkIO func
  threadDelay (8400*second_us)
  killThread threadID



func = do
  putStrLn $ "length ad0:\t" ++ (show $ length ad0)
  putStrLn $ unlines $ map show $ map length $ map (basisElements [a, b] (map leadingMonomial ad0)) $ [1..5]
  putStrLn $ "dimensions of ad0 \t"
  putStrLn $ unlines $ map show $ zip [1..5] $ map  (countDimension ad0) [1..5]

  putStrLn $ "length ad1:\t" ++ (show $ length ad1)
  putStrLn $ unlines $ map show $ map length $ map (basisElements [a, b] (map leadingMonomial ad1)) $ [1..5]
  putStrLn $ "dimensions of ad1 \t"
  putStrLn $ unlines $ map show $ zip [1..5] $ map  (countDimension ad1) [1..5]
  -- putStrLn $ "length nub (map leadingMonomial) ad1:\t" ++ (show $ length $ nub $ map leadingMonomial ad1)
  putStrLn $ "length ad2:\t" ++ (show $ length ad2)
  putStrLn $ unlines $ map show $ map length $ map (basisElements [a, b] (map leadingMonomial ad2)) $ [1..5]
  putStrLn $ "dimensions of ad2 \t"
  putStrLn $ unlines $ map show $ zip [1..5] $ map  (countDimension ad2) [1..5]
  -- putStrLn $ "length nub (map leadingMonomial) ad2:\t" ++ (show $ length $ nub $ map leadingMonomial ad2)
  putStrLn $ "length ad3:\t" ++ (show $ length ad3)
  -- putStrLn $ "length nub (map leadingMonomial) ad3:\t" ++ (show $ length $ nub $ map leadingMonomial ad3)
  putStrLn $ unlines $ map show $ map length $ map (basisElements [a, b] (map leadingMonomial ad3)) $ [1,2,3,4,5]

