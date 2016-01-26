module Main where

import Data.Ix
import Data.List
import Control.Applicative
import Control.Monad.Random
import System.Environment
import System.Exit
import System.IO

-- Tree data structure -----------------------------
data Tree a = Empty | Branch (Tree a) a (Tree a)

-- Input/Output of trees
instance Show a => Show (Tree a) where
  show (Branch left val right) = "(" ++ show left ++
                                 "." ++ show val ++
                                 "." ++ show right ++
                                 ")"
  show Empty = ""

instance Read a => Read (Tree a) where
  readsPrec _ s = readsTree s

readsTree :: Read a => ReadS (Tree a)
readsTree ('(':s) = [(Branch l v r, z) | (l, '.':t) <- reads s,
                     (v, '.':u) <- reads t,
                     (r, ')':z) <- reads u]
readsTree s = [(Empty, s)]

-- Tree generation ---------------------------------
genTree :: [a] -> Rand StdGen (Tree a)
genTree sequ@(x:xs) =
  do
    rootidx <- getRandomR (0, length sequ - 1)
    left <- genTree $ take rootidx sequ
    right <- genTree $ drop (rootidx+1) sequ
    return $ Branch left (sequ !! rootidx) right
genTree sequ = return Empty

-- Sequence generation -----------------------------
genSeq :: (Random a, Ord a, Ix a) => Int -> (a, a) -> Rand StdGen [a]
genSeq n bounds =
  case compare (rangeSize bounds) n of
   LT -> undefined
   EQ -> return $ range bounds
   GT -> (sort . take n . nub) <$> getRandomRs bounds

-- Test case generation ----------------------------
genCase :: (Random a, Ord a, Ix a) => Int -> (a, a) -> Rand StdGen (Tree a)
genCase n bounds =
  genSeq n bounds >>= genTree

-- Main : argument parsing + test case generation --

main = do
  args <- getArgs
  case args of
   [nString, minString, maxString, seedString] | [(n, "")] <- reads nString,
                                                 [(mi, "")] <- reads minString,
                                                 [(ma, "")] <- reads maxString,
                                                 [(seed, "")] <- reads seedString ->
                                                   let
                                                     g = mkStdGen seed
                                                     tree = evalRand (genCase n (mi, ma)) g:: Tree Int
                                                   in
                                                    putStrLn . show $ tree
   _ -> do
     name <- getProgName
     hPutStrLn stderr $ "usage: " ++ name ++ " <n> <min> <max> <seed>"
     exitFailure
