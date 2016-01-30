module Main where

import Control.Applicative
import Control.Monad.Random
import System.Environment
import System.Exit
import System.IO

genStr :: Int -> Rand StdGen String
genStr n = (concat . map show . take n) <$> getRandomRs (1 :: Int, 9)

next' s@(c:rest) acc =
  let (cs, others) = span (==c) s
      l = length cs
  in
   case (show l) of
    (cl:[]) -> next' others (c:cl:acc)
    _ -> undefined

next' _ acc = reverse acc

next :: String -> String
next s = next' s ""

genCase' 0 s = s
genCase' k s = genCase' (k-1) (Main.next s)

genCase :: Int -> Int -> Rand StdGen String
genCase n k = genCase' k <$> genStr n

main =
  do
    args <- getArgs
    case args of
     [nString, kString, seedString] | [(n, "")] <- reads nString,
                                      [(k, "")] <- reads kString,
                                      [(seed, "")] <- reads seedString ->
                                      let g = mkStdGen seed
                                          s = evalRand (genCase n k) g :: String
                                      in putStrLn s
     _ -> do
       name <- getProgName
       hPutStrLn stderr $
         "usage: " ++ name ++ " <n> <k> <seed>"
       exitFailure
