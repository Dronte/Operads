module Main where

import Math.Operad 
import Data.List (nub,delete)
import Control.Concurrent
import Debug.Trace

a = corolla 1 [1,2]
b = corolla 2 [1,2]

ts :: [OperadElement Integer Rational PathRPerm]
ts = map oet
     [shuffleCompose 1 [1,2,3] a a, shuffleCompose 2 [1,2,3] a a, shuffleCompose 1 [1,3,2] a a, 
      shuffleCompose 2 [1,2,3] a b, shuffleCompose 1 [1,2,3] a b, shuffleCompose 1 [1,3,2] b a, 
      shuffleCompose 2 [1,2,3] b a, shuffleCompose 1 [1,3,2] b b, shuffleCompose 1 [1,3,2] a b, 
      shuffleCompose 1 [1,2,3] b a, shuffleCompose 2 [1,2,3] b b, shuffleCompose 1 [1,2,3] b b]
opSum = foldr (+) zero
g1 = opSum $ zipWith (.*.) [1,-1,1,-1] $ map ((ts!!) . (subtract 1)) [1,2,11,12]
--g2 = opSum $ zipWith (.*.) [1,-1,1,-1] $ map ((ts!!) . (subtract 1)) [5,6,9,10]
--g3 = opSum $ zipWith (.*.) [1,-1,1,-1] $ map ((ts!!) . (subtract 1)) [7,8,3,4]
--ad0 = [g1,g2,g3]
--adn1 = stepOperadicBuchberger [] ad0
--ad1 = nub $ ad0 ++ adn1
--adn2 = stepOperadicBuchberger ad0 adn1
--ad2 = nub $ ad1 ++ adn2
--adn3 = stepOperadicBuchberger ad1 adn2
--ad3 = nub $ ad2 ++ adn3

makeBasis :: (TreeOrdering t) => Int->[OperadElement Integer Rational t ] -> [[OperadElement Integer Rational t]]
makeBasis n ts =
  makeBasis' n ts [0..(length ts -1)]
  where
--    makeBasis' :: (Num a,TreeOrdering t) => a->[OperadElement Integer Rational t ]
--                  ->[Int]-> [[OperadElement Integer Rational t]]
    makeBasis' 0 _ _ = [[]]
    makeBasis' n ts ind =concat $ map foreach ind
      where
        -- foreach :: (TreeOrdering t1) => Int -> [[OperadElement Integer Rational t1]]
        foreach i = map ((ts !! i):) $ makeBasis' (n-1) ts (delete i ind)
        --foreach i = [(ts!!i)] ++ makeBasis' (n-1) ts (delete i ind)

monomial n = makeBasis n ts

--steps :: (TreeOrdering t) => [OperadElement Integer Rational t ] -> [[OperadElement Integer Rational t]]
steps monoms = [fst a|
              a<-generateList [(ad0,stepOperadicBuchberger [] ad0)]]
    where
      ad0  = map (\x->opSum([x])) monoms
      basis (ad,adn) = nub $ ad ++ adn
      step  (ad,adn) = stepOperadicBuchberger ad adn
      generateList list = list ++ [(basis a,step a) | a<-generateList list]

hilbertSeries n ad = map length $ map (basisElements [a,b] (map leadingMonomial ad)) $ [1..n]

main = do
  putStrLn $ show $ map (hilbertSeries 5) $ monomial 2

