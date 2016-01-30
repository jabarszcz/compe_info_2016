module Main where

import Data.Function
import Data.List
import Data.Ord
import qualified Data.Map.Lazy as Map

-- import Debug.Trace

-- Le coeur de la solution consiste à voir que le problème présente
-- une situation avec une "sous-structure optimale".
-- (https://en.wikipedia.org/wiki/Optimal_substructure) En considérant
-- cela, on peut arriver à une solution qui utilise la programmation
-- dynamique: on mémoise les résultats à chaque sous-problème et on
-- résout récursivement.

-- L'implémentation écrite ici réalise la mémoisation à l'aide d'une
-- map sur un domaine donné en utilisant la paresse de Haskell. En
-- formulant la récursion de façon à explicitement passer la fonction
-- utilisée pour "récurser" en argument à cost', on peut passer la
-- fonction mémoisée et ainsi éviter la redondance des calculs lors de
-- la récursion. Le combinateur "fixm" permet de joindre tous ces
-- concepts.


-- Memoization --------------------------------------------------------
map_memoize b f = ((Map.fromList (map (\e -> (e, f e)) b)) Map.!)
fixm memoization f = fix (memoization . f)

-- Competition utils --------------------------------------------------

lineToInts :: String -> [Int]
lineToInts = (map read) . words

-- Solution -----------------------------------------------------------

-- Recursive cost function
-- Finds the optimal search cost in a subsequence
-- cost' sequence func (start_idx, end_idx)
--     parameter sequence : the sequence of ints to search from
--     parameter func : The function we recurse into (with fix)
--     parameter (start_idx, end_idx) bounds (indices) of the subsequence
cost' :: (Integral a, Show a) => [a] -> ((Int, Int) -> a) -> (Int, Int) -> a
cost' _ _ (i, 0) = 0
cost' _ _ (i, 1) = 0
cost' s f (i, n) | i >= n = 0
                 | otherwise = --if i==1 then
                                 --trace (">"++(show (s!!(i-1),s!!(m-1),s!!(n-1)))++" is "++(show result)) $
                                 result
                               --else result
  where
  lower u = f (i, u)
  upper l = f (l, n)
  worst m' = fromIntegral $ max (lower (m'-1)) (upper (m'+1))
  m = let indicesCosts = [(m', worst m') | m' <- reverse [i..n]]
          index = fst $ minimumBy (comparing $ \(i, w) -> (s!!(i-1) + w)) $ indicesCosts
      in
    index
  result = (s !! (m-1)) + (worst m)

-- Main and Parsing ---------------------------------------------------

main =
  do
    lengths:mySequenceLine:caseLines <- fmap lines $ getContents
    top:ncases:_ <- return $ lineToInts lengths
    mysequence <- return $ lineToInts mySequenceLine
    cases <- return $ map read caseLines
    if (ncases > 0) then
      let
        domain = [(i, n) | n <- [0..top], i <- [1..n+1]]
        
        cost'' = cost' mysequence
        cost_memoized = (fixm (map_memoize domain) cost'')
        cost n = cost_memoized (1, n)
      in
       mapM_ (putStrLn . show . cost) cases
      else return ()
    return ()
