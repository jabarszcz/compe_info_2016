module Main where

import Data.List
import Control.Monad
import Control.Monad.Random
import System.Environment
import System.Exit
import System.IO

-- Generation of sequences ------------------------
genSeq :: (Integral a, Random a) => Int -> Int -> Rand StdGen [a]
genSeq n s =
  liftM (sort . take n . nub) (getRandomRs (1, (fromIntegral $ n + s )))

genSeq2 :: (Integral a, Random a) => Int -> Int -> Rand StdGen [a]
genSeq2 k n =
  liftM (take k) (getRandomRs (1, fromIntegral n))

main = do
  args <- getArgs
  case args of
   [sN,
    sSparseness,
    sK,
    seedString] | [(n, "")] <- reads sN,
                  [(k, "")] <- reads sK,
                  [(s, "")] <- reads sSparseness,
                  [(seed, "")] <- reads seedString ->
                    let
                      g = mkStdGen seed
                      sequ =
                        evalRand
                        (genSeq (n :: Int) s)  g :: [Integer]
                      sequ2 =
                        evalRand
                        (genSeq2 k n) g :: [Integer]
                    in do
                      putStrLn . unwords $ map show [n, k]
                      putStrLn . unwords $ map show sequ
                      mapM_ (putStrLn . show) $ sequ2
   _ -> do
     name <- getProgName
     hPutStrLn stderr $ "usage: " ++ name ++ " <n> <k> <s> <seed>"
     exitFailure
