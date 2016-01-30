module Main where

import Data.Ix
import Data.List
import Data.Numbers.Primes
import Control.Applicative
import Control.Monad
import Control.Monad.Random
import System.Environment
import System.Exit
import System.IO

-- Data structures ---------------------------------------
data Digits a = DigitsR [a] | DigitsL [a]

extract (DigitsR ds) = ds
extract (DigitsL ds) = ds

reverseDigits (DigitsR ds) = DigitsL (reverse ds)
reverseDigits (DigitsL ds) = DigitsR (reverse ds)

right l@(DigitsL _) = reverseDigits l
right r = r

left = reverseDigits . right

padDigits n digits = DigitsL (
  take (fromInteger n) $ (extract $ left digits) ++ [0 | _ <- [1..]])

-- IO ---------------------------------------------

-- Reading --
instance (Read a, Show a) => Read (Digits a) where
  readsPrec _ = readsDigits

readsDigits :: (Read a, Show a) => ReadS (Digits a)
readsDigits ('(':s) = [(DigitsR (d:ds), r) |
                       (d, ')':t) <- reads s,
                       (DigitsR ds, r) <- reads t]
                      ++
                      [(DigitsR [d], r) |
                       (d, ')':r) <- reads s]
readsDigits (sd:sds) = [(DigitsR (d:ds), r) |
                        (d, _) <- reads [sd],
                        (DigitsR ds, r) <- reads sds]
                       ++
                       [(DigitsR [d], sds) |
                        (d, _) <- reads [sd]]
readsDigits _ = []

-- Showing --
instance (Show a) => Show (Digits a) where
  show digits =
    let showOne cs@(c1:c2:cmore) = "(" ++ cs ++ ")"
        showOne cs = cs
    in
     concatMap (showOne . show) (extract $ right digits)

-- Translations -----------------------------------

baseToIntegral :: Integral a => a -> Digits a -> a
baseToIntegral base (DigitsL (d:ds)) =
  d + base * baseToIntegral base (DigitsL ds)
baseToIntegral base (DigitsL _) = 0
baseToIntegral base digits = baseToIntegral base (left digits)

integralToBase :: Integral a => a -> a -> Digits a
integralToBase base 0 = DigitsL [0]
integralToBase base i =
  let (d, m) = divMod i base in
   case d of
    0 -> DigitsL [m]
    otherwise -> DigitsL (m:(extract . left $ integralToBase base d))

-- Modular ----------------------------------------
  
digitsModulo :: (Integral a) => a -> Digits a -> Digits a
digitsModulo n digits = DigitsL (take (fromIntegral n) . extract $
                                 left digits)

-- Prime number generation ------------------------
genPrime :: (Integral a, Ix a, Random a) => (a, a) -> Rand StdGen a
genPrime bounds =
  genericIndex primes <$> (getRandomR bounds)

-- Generation of multiples ------------------------
genMult :: (Integral a, Random a) => a -> a -> (a, a) -> Rand StdGen [Digits a]
genMult prime base bounds =
  liftM (map (integralToBase base . (*prime))) (getRandomRs bounds)

-- Test case generation ----------------------------
genCase :: (Integral a, Ix a, Random a) => a -> Int -> (a, a) -> (a, a) -> Rand StdGen (Digits a, [Digits a])
genCase base k pbounds mbounds = -- a prime and multiples
  do
    p <- genPrime pbounds
    mults <- take k <$> genMult p base mbounds
    return (integralToBase base p, mults)

-- Main : argument parsing + test case generation --

main = do
  args <- getArgs
  case args of
   [sBase,
    sNDigits,
    sKMultiples,
    sMinP, sMaxP,
    sMinM, sMaxM,
    seedString] | [(b, "")] <- reads sBase,
                  [(n, "")] <- reads sNDigits,
                  [(k, "")] <- reads sKMultiples,
                  [(pmin, "")] <- reads sMinP,
                  [(pmax, "")] <- reads sMaxP,
                  [(mmin, "")] <- reads sMinM,
                  [(mmax, "")] <- reads sMaxM,
                  [(seed, "")] <- reads seedString ->
                    let
                      g = mkStdGen seed
                      pbounds = (pmin, pmax)
                      mbounds = (mmin, mmax)
                      (p, multiples) =
                        evalRand
                        (genCase (b :: Int) k pbounds mbounds) g
                    in do 
                      putStrLn . unwords $ (show p:map show [b, n, k])
                      mapM_ (putStrLn . show) multiples
   _ -> do
     name <- getProgName
     hPutStrLn stderr $ "usage: " ++ name ++ " <b> <n> <k> <pmin> <pmax> <mmin> <mmax> <seed>"
     exitFailure
