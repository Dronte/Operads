module Main where

import Math.Operad 
--import Math.Operad.OrderedTree (TreeOrdering)
import Data.List (nub)
import Control.Concurrent

a = corolla 1 [1,2]
b = corolla 2 [1,2]
-- | Using the path sequence, the leaf orders and order reversal, we can get 8 different orderings
-- from one paradigm. These are given by 'PathPerm', 'RPathPerm', 'PathRPerm', 'RPathRPerm' for the 
-- variations giving (possibly reversed) path sequence comparison precedence over (possibly reversed)
-- leaf permutations; additionally, there are 'PermPath', 'RPermPath', 'PermRPath' and 'RPermRPath'
-- for the variations with the opposite precedence.

tsGenerator :: TreeOrdering t => [OperadElement Integer Rational t]
tsGenerator = map oet
  [shuffleCompose 1 [1,2,3] a a, shuffleCompose 2 [1,2,3] a a, shuffleCompose 1 [1,3,2] a a, 
   shuffleCompose 2 [1,2,3] a b, shuffleCompose 1 [1,2,3] a b, shuffleCompose 1 [1,3,2] b a, 
   shuffleCompose 2 [1,2,3] b a, shuffleCompose 1 [1,3,2] b b, shuffleCompose 1 [1,3,2] a b, 
   shuffleCompose 1 [1,2,3] b a, shuffleCompose 2 [1,2,3] b b, shuffleCompose 1 [1,2,3] b b]

  
flex :: TreeOrdering t => [OperadElement Integer Rational t] -> [[OperadElement Integer Rational t]]
flex ts = [fst a |
                   a<-generateList[(ad0,stepOperadicBuchberger [] ad0)]]
    where
    opSum = foldr (+) zero
    g1    = opSum $ zipWith (.*.) [1,-1,1,-1] $ map ((ts!!) . (subtract 1)) [1,2,11,12]
    g2    = opSum $ zipWith (.*.) [1,-1,1,-1] $ map ((ts!!) . (subtract 1)) [5,6,9,10]
    g3    = opSum $ zipWith (.*.) [1,-1,1,-1] $ map ((ts!!) . (subtract 1)) [7,8,3,4]
    ad0  = [g1,g2,g3]
    basis (ad,adn) = nub $ ad ++ adn
    step  (ad,adn) = stepOperadicBuchberger ad adn
    --generateList   = [([],adn0)] ++ [ (basis a,step a) | a<-generateList] 
    generateList list = list ++ [(basis a,step a) | a<-generateList list]

counter :: TreeOrdering t => [OperadElement Integer Rational t] -> String -> IO ()
counter ts name = do
  result <- return $ flex ts
  loop result 1
       where
         loop result number= do
           putStrLn $ "Length " ++ name ++" "++ (show $ number) ++ " "++(show $ length $ head result )
           loop (tail result) (number+1)

second_us = 1000000
main = do
  forkIO $ counter (tsGenerator :: [OperadElement Integer Rational PathPerm]) "PathPerm"
  forkIO $ counter (tsGenerator :: [OperadElement Integer Rational PathRPerm]) "PathRPerm"
  forkIO $ counter (tsGenerator :: [OperadElement Integer Rational RPathPerm]) "RPathPerm"
  forkIO $ counter (tsGenerator :: [OperadElement Integer Rational RPathRPerm]) "RPathRPerm"
  forkIO $ counter (tsGenerator :: [OperadElement Integer Rational PermPath]) "PermPath"
  forkIO $ counter (tsGenerator :: [OperadElement Integer Rational PermRPath]) "PermRPath"
  forkIO $ counter (tsGenerator :: [OperadElement Integer Rational RPermPath]) "RPermPath"
  forkIO $ counter (tsGenerator :: [OperadElement Integer Rational RPermRPath]) "RPermRPath"
  threadDelay (30000*second_us)


--counter = do
    --putStrLn $ show $ length $ flex (tsGenerator :: [OperadElement Integer Rational PathRPerm])
--  putStrLn $ "length ad1:\t" ++ (show $ length ad1)
--  putStrLn $ "length ad2:\t" ++ (show $ length ad2)
--  putStrLn $ "length ad3:\t" ++ (show $ length ad3)
--  putStrLn $ "ad2 == ad3:\t" ++ (show $ ad2 == ad3)
--  putStrLn $ "length nub (map leadingMonomial) ad1:\t" ++ (show $ length $ nub $ map leadingMonomial ad1)
--  putStrLn $ "length nub (map leadingMonomial) ad2:\t" ++ (show $ length $ nub $ map leadingMonomial ad2)
--  putStrLn $ "length nub (map leadingMonomial) ad3:\t" ++ (show $ length $ nub $ map leadingMonomial ad3)
--  putStrLn $ unlines $ map show $ operadicBuchberger ad0
--  putStrLn $ unlines $ map show $ map length $ map (basisElements [a, b] (map leadingMonomial ad3)) $ [1,2,3,4,5]
