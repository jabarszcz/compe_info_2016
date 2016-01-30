module Main (predecessor, first, main) where

import Control.Applicative
import Data.Char

-- Structure de données pour un terme possiblement existant. Un Maybe
-- String aurait fait l'affaire ici.
data Term = Term String | Nil


instance Read Term where
  readsPrec _ s = (\(digits,rest) -> [(Term digits, rest)]) $ span (liftA2 (&&) isDigit (/='0')) s

instance Show Term where
  show (Term s) = s
  show Nil = ":(" -- "Not-a-Term"

-- On calcule le prédécésseur en le construisant (tail-)récursivement
-- à l'envers dans un accumulateur
predecessor' :: Term -> Char -> Term -> Term

-- Première itération
predecessor' (Term s@(n:c:rest)) _ Nil =
    predecessor' (Term rest) c (Term (replicate (read [n]) c))

-- On ne peux pas décrire deux fois le même chiffre de suite alors on
-- retourne nil si ça arrive.
predecessor' (Term (n:c:rest)) last (Term acc) =
  if c == last
  then Nil
  else
    -- On construit le nouveau terme à l'envers pour aller plus vite
    predecessor' (Term rest) c (Term (replicate (read [n]) c ++ acc))

-- On remet la liste en ordre quand on finit la récursion.
predecessor' (Term []) _ (Term acc) =
  Term (reverse acc)

-- Un prédécesseur n'existe pas si le terme courant a un nombre impair
-- de chiffres.
predecessor :: Term -> Term
predecessor t@(Term s) =
  if odd $ length s
  then Nil
  else 
    predecessor' t undefined Nil

--  Trouve (tail-)récursivement le terme de la suite qui n'a pas de
--  prédécesseur.
first :: Term -> Term
first t@(Term s) = 
  -- Le terme 22 est le seul qui se répète à l'infini.
  if s == "22"
  then Nil
  else
    case predecessor t of
     Nil -> t
     p@(Term _) -> first p


-- La beauté de la composition et des formulations "point-free":
main =  getContents >>= (putStrLn . show . first . read . head . lines)
