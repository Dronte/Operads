module Utils 
    where

import Math.Operad 
import Data.List (nub)

--shuffleIdentiy :: (Ord a,Show a) => DecoratedTree a -> Shuffle -> OperadElement
--shuffleIdentiy coefficents symmetricTrees =
--    [opSum $ zipWith (.*.) coefficents symmetricTree
--                                                 | symmetricTree <- symmetricTrees ] 
--shuffleIdentity [1,1,1] $ map (ts !!) [1,2,3]


numberOfLeafs t = case t of DTLeaf _ -> 1
                            DTVertex _ subTr -> sum $ map numberOfLeafs $ subTrees t

countDimension ad i = length $ filter (==i) $ map numberOfLeafs $ map leadingMonomial ad
