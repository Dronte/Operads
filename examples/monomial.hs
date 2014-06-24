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

makeBasis :: (TreeOrdering t) => Int->[OperadElement Integer Rational t ] -> [[OperadElement Integer Rational t]]
makeBasis n ts =
  makeBasis' n ts [0..(length ts -1)]
  where
    makeBasis' 0 _ _ = [[]]
    makeBasis' n ts ind =concat $ map foreach ind
      where
        foreach i = map ((ts !! i):) $ makeBasis' (n-1) ts (delete i ind)

monomial n = makeBasis n ts

--steps :: (TreeOrdering t) => [OperadElement Integer Rational t ] -> [[OperadElement Integer Rational t]]
steps monoms = [fst a|
              a<-generateList [(ad0,stepOperadicBuchbergerCompletely [] ad0)]]
    where
      opSum = foldr (+) zero   
      ad0  = map (\x->opSum([x])) monoms
      basis (ad,adn) = nub $ ad ++ adn
      step  (ad,adn) = stepOperadicBuchbergerCompletely ad adn
      generateList list = list ++ [(basis a,step a) | a<-generateList list]

hilbertSeries n ad = map length $ map (basisElements [a,b] (map leadingMonomial ad)) $ [1..n]

main = do
  putStrLn $ unlines $map (\x->(pp $ fst x)++(show $ snd x)) $ map (\x->(x,hilbertSeries 5 x)) $ monomial 3

