module Main where

-- Un structure de données pour garder les chiffres du nombre.
--
-- DigitsR les garde dans l'ordre de lecture/écriture alors que
-- DigitsL les garde dans l'ordre croissant de magnitude.
data Digits a = DigitsR [a] | DigitsL [a]

-- Obtenir la liste des chiffres
extract (DigitsR ds) = ds
extract (DigitsL ds) = ds

-- Obtenir l'ordre inverse
reverseDigits (DigitsR ds) = DigitsL (reverse ds)
reverseDigits (DigitsL ds) = DigitsR (reverse ds)

-- Obtenir les chiffres dans l'ordre de lecture/écriture
right l@(DigitsL _) = reverseDigits l
right r = r

-- Obtenir les chiffres dans l'ordre croissant de magnitude
left = reverseDigits . right

-- Obtenir un certain nombre de chiffres en tronquant ou en ajoutant
-- des zéros comme chiffres de plus grande magnitude
padDigits n digits = DigitsL (
  take (fromIntegral n) $ (extract $ left digits) ++ [0 | _ <- [1..]])

-- IO ---------------------------------------------

-- Reading

-- Les fonctions de lecture ne sont pas formulées de façon élégante,
-- ni en suivant la formulation habituelle des fonctions reads. C'est
-- que ce fonctions deviennent lentes pour de grandes entrées, et que
-- nous avons besoin de faire une lecture rapide.

-- The reading functions are not as elegant as they could be. The
-- usual idiom for reading with a normal reads function was to slow.

instance (Read a, Show a) => Read (Digits a) where
  readsPrec _ = readsDigits

readsDigits :: (Read a, Show a) => ReadS (Digits a)
readsDigits s = [(DigitsR (readDigits s), "")]

readDigits :: (Read a) => String -> [a]
readDigits string = readDigits' [] string

readDigits' :: (Read a) => [a] -> String -> [a]
readDigits' digits ('(':rest) =
  readDigits'' digits [] rest
readDigits' digits (c:rest) =
  readDigits' (read [c]:digits) rest
readDigits' digits _ = reverse digits

readDigits'' :: (Read a) => [a] -> String -> String -> [a]
readDigits'' digits chars (')':rest) =
  readDigits' (read (reverse chars) : digits) rest
readDigits'' digits chars (c:rest) =
  readDigits'' digits (c:chars) rest
readDigits'' digits chars _ = undefined

-- Showing --
instance (Show a) => Show (Digits a) where
  show digits =
        -- plusieurs caratères entre paranthèses
    let showOne cs@(c1:c2:cmore) = "(" ++ cs ++ ")"
        -- un caractère passe sans changement
        showOne cs = cs
    in
     concatMap (showOne . show) (extract $ right digits)

-- Changements de base ----------------------------

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

-- Arithmétique modulaire  -----------------------

-- Modulo des digits dans leur base == tronquer des chiffres
digitsModulo :: (Integral a) => a -> Digits a -> Digits a
digitsModulo n digits = DigitsL (take (fromIntegral n) . extract $
                                 left digits)

-- Trouver l'inverse modulaire est utile pour rapidement trouver les
-- facteurs de plusieurs multiples en multipliant de petits nombres au
-- lieu de diviser de grands nombres.
inverse :: (Integral a, Show a) => a -> a -> a
inverse q p = fst $ inverse' q p
  where
    inverse' _ 0 = error ("division by " ++ show p ++
                          " (mod " ++ show q ++
                          "), non-coprime modulus")
    inverse' _ 1 = (1, 0)
    inverse' q p =
      let (m, r) = divMod q p
          (r', rr) = inverse' p r
      in
       (m * p + r - m * r' - rr, p-r')

-- Main -------------------------------------------

main = do
  (constantsS:multiplesS) <- getContents >>= return . lines
  let
    (ps:otherConstS) = words constantsS
    pd = read ps :: Digits Int
    (b:n:k:_) = map read $ otherConstS :: [Int]
    m = b^n
    p = baseToIntegral b pd
    p' = inverse m (baseToIntegral b . digitsModulo n $ pd)
    in
   mapM_ --(naive p b n m)
   (putStrLn . show . padDigits n . integralToBase b .
    (*p') . baseToIntegral b . digitsModulo n . read)
   multiplesS

-- Approche naive et plus lente de division ------
naive p b n m =
  (putStrLn . show . padDigits n . integralToBase b . (`mod` m) .
   (`div` p) . baseToIntegral b . read)
