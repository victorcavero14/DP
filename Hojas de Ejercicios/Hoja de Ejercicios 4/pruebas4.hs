import Data.List
import Data.Char
{-
    EJERCICIO 1
-}
data Pila a = P [a] deriving (Show)

creaPila :: Pila a
creaPila = P []

esPilaVacia :: Pila a -> Bool
esPilaVacia (P xs)
    | null xs = True
    | otherwise = False

apilar :: a -> Pila a -> Pila a --ojo te devuelve una pila nueva
apilar x (P xs) = P $ x : xs

cima :: Pila a -> a
cima (P []) = error "Pila vacia"
cima (P xs) = head xs

desapilar :: Pila a -> Pila a
desapilar (P []) = P []
desapilar (P xs) = P $ tail xs 

{- Que significa esta fun: 

Va apilando la lista siguiendo el propio orden de la lista, de manera que como
en una pila los elementos siempre se ponen en la cima esta queda ordenada de forma inversa.

Como elemento neutro tiene la pila vacía y la lambda que va apilando.

-}

r :: [a] -> [a]
r xs = ys 
    where P ys = foldl (\p x -> apilar x p) creaPila xs

{-
    EJERCICIO 2

    Data Maybe a = Nothing | Just a
-}

primeroQueCumple :: (a -> Bool) -> [a] -> Maybe a
primeroQueCumple f [] = Nothing 
primeroQueCumple f (x:xs) 
    | f x = Just x
    | otherwise = primeroQueCumple f xs

{-
    EJERCICIO 3, ES MUY PARECICDO AL 1
-}

{-
    EJERCICIO 4
-}



