module Main where

import Control.Applicative

-- La structure de donnée récursive d'arbre.
--
-- Chaque arbre est soit une branche avec deux sous-arbres et une
-- valeur interne ou est vide.
data Tree a = Empty | Branch (Tree a) a (Tree a)

-- Input/Output des arbres
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

-- Calcul récursif du coût maximal de recherche
cost Empty = 0 -- Un arbre vide est gratuit
cost (Branch Empty _ Empty) = 0 -- On ne compte pas les feuilles
cost (Branch left val right) = max (cost left) (cost right) + val

-- On utilise le nombre de noeuds pour calculer le nombre d'arbres
-- binaires de recherche. Il est simplement calculé récursivement.
nodeCount Empty = 0
nodeCount (Branch l _ r) = 1 + nodeCount l + nodeCount r


-- Nombre d'arbres binaire de recherche de longeur donnée
-- 
-- Le calcul du nombre d'arbres de recherche binaire doit être
-- efficace parce que le nombre d'éléments sera grand. Il correspond
-- au nombre catalan d'indice n pour un arbre binaire de longueur n.
--
-- Les nombres catalans peuvent se calculer avec un coefficient
-- binomial.  Une façon de calculer rapidement ces coefficients se
-- trouve ici:
--
-- http://rosettacode.org/wiki/Evaluate_binomial_coefficients#Haskell
--
-- Il faut voir qu'on accumule une fraction en divisant toujours le
-- numérateur par le nouveau dénominateur, gardant ainsi le produit
-- relativement petit. Aussi, cette division sera toujours entière, ce
-- qui fait qu'on peut utiliser div.
choose n k = foldl (\z i -> (z * (n-i+1)) `div` i) 1 [1..k]

-- Les nombres catalans sont calculés à partir de la formule avec le
-- coefficent binominal. On peut trouver cette formule ici:
--
-- https://en.wikipedia.org/wiki/Catalan_number
catalan n = choose (2*n) n `div` (n+1)

-- On met le tout ensemble
cousinBSTsCount tree = catalan $ nodeCount tree

-- Main
main = do
  tree <- (read . head . lines) <$> getContents
  mapM_ (putStrLn . show . ($ tree)) $ [cost, cousinBSTsCount]
