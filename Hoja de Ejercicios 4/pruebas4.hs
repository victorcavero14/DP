
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys


data Pila = P [Integer] deriving (Show)

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs
 

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

sumame4 x = if (x < 100) then x + 4 else x
sumame3 x = x + 3
--esPilaVacia

--apilar

--desapilar