module Main where

import Math.Operad 
import Data.List (nub)
import Control.Concurrent
a = corolla 1 [1,2] 
b = corolla 2 [1,2]

ts :: [OperadElement Integer Rational RPathPerm]
ts = map oet
     [shuffleCompose 1 [1,2,3] a a, shuffleCompose 2 [1,2,3] a a, shuffleCompose 1 [1,3,2] a a,
      shuffleCompose 2 [1,2,3] a b, shuffleCompose 1 [1,2,3] a b, shuffleCompose 1 [1,3,2] b a, 
      shuffleCompose 2 [1,2,3] b a, shuffleCompose 1 [1,3,2] b b, shuffleCompose 1 [1,3,2] a b, 
      shuffleCompose 1 [1,2,3] b a, shuffleCompose 2 [1,2,3] b b, shuffleCompose 1 [1,2,3] b b]
opSum = foldr (+) zero
jacobi= opSum $ zipWith (.*.) [1,-1,-1] $ map ((ts!!) . (subtract 1)) [1,2,3]
-- It's not beautifull to write [0,1,2] that's why we call subtract
identyty lst = opSum $ zipWith (.*.) [-1,1,1] $ map ((ts!!) . (subtract 1)) lst
identyties = [  identyty [1,2,4],
                identyty [3,4,2],
                identyty [10,6,8],
                identyty [9,10,12],
                identyty [7,8,6],
                identyty [11,12,10]]
ad0 = identyties
adn1 = stepOperadicBuchbergerLtOnly [] ad0
ad1 = nub $ ad0 ++ adn1
adn2 = stepOperadicBuchbergerLtOnly ad0 adn1
ad2 = nub $ ad1 ++ adn2
adn3 = stepOperadicBuchbergerLtOnly ad1 adn2
ad3 = nub $ ad2 ++ adn3

second_us = 1000000
main = do
  threadID <- forkIO func
  threadDelay (300*second_us)
  killThread threadID


func = do
  putStrLn $ "length ad1:\t" ++ (show $ length ad1)
  putStrLn $ "length ad2:\t" ++ (show $ length ad2)
  putStrLn $ "length ad3:\t" ++ (show $ length ad3)
  putStrLn $ "ad2 == ad3:\t" ++ (show $ ad2 == ad3)
  putStrLn $ "length nub (map leadingMonomial) ad1:\t" ++ (show $ length $ nub $ map leadingMonomial ad1)
  putStrLn $ "length nub (map leadingMonomial) ad2:\t" ++ (show $ length $ nub $ map leadingMonomial ad2)
  putStrLn $ "length nub (map leadingMonomial) ad3:\t" ++ (show $ length $ nub $ map leadingMonomial ad3)
  putStrLn $ unlines $ map show $ operadicBuchberger ad0
  putStrLn $ unlines $ map show $ map length $ map (basisElements [a] (map leadingMonomial ad3)) $ [1,2,3,4,5,6,7,8]
