module Utils 
    where

import Math.Operad 
import Data.List (nub)

--shuffleIdentiy :: (Ord a,Show a) => DecoratedTree a -> Shuffle -> OperadElement
--shuffleIdentiy coefficents symmetricTrees =
--    [opSum $ zipWith (.*.) coefficents symmetricTree
--                                                 | symmetricTree <- symmetricTrees ] 
--shuffleIdentity [1,1,1] $ map (ts !!) [1,2,3]
reduceList ad = loop [] (head ad) (tail ad)
  where
    loop prefix x [] = prefix++[reduceLtOnly x prefix]
    loop prefix x suffix =
      loop (prefix++[reduceLtOnly x (prefix++suffix)]) (head suffix) (tail suffix)

reduceTotaly ad = reduceTotaly' ad []
  where
    reduceTotaly' ad oldad = if ad == oldad then filter (not . isZero) ad
                     else reduceTotaly' (reduceList ad) ad


numberOfLeafs t = case t of DTLeaf _ -> 1
                            DTVertex _ subTr -> sum $ map numberOfLeafs $ subTrees t

countDimension ad i = length $ filter (==i) $ map numberOfLeafs $ map leadingMonomial ad
