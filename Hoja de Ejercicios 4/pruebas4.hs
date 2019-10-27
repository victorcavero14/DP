maximum' :: (Num a, Ord a) => [a] -> a
maximum' [] = error "Empty List"
maximum' (x:[]) = x
maximum' (x:xs) = if (x < maximum' xs) then maximum' xs else x


data Pila = P [Integer] deriving (Show)


sumame4 x = if (x < 100) then x + 4 else x
sumame3 x = x + 3
--esPilaVacia

--apilar

--desapilar