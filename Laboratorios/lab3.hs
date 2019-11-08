import Data.List

--Victor Manuel Cavero Gracia, 3A

--1

--a
listacontrarios :: [Integer]
listacontrarios = [x*(-1)^n | x <- [1..], n <- [2,1]]

--b
listapares :: [(Integer,Integer)]
listapares = concat [parejasDesde (x) | x <- [0..]]

parejasDesde :: (Num a, Enum a) => a -> [(a,a)]
parejasDesde x = [(z,y) | y <- [x, x-1 .. 0], z <- [(x - y)] ]

--2

--a
sufijos :: [a] -> [[a]]
sufijos xs = [drop i xs | i <- [0..length xs]]

--b

sublists :: [a] -> [[a]]
sublists ls = [x | y <- inits ls, x <- tails y, not(null x)]

--c
    
--perms = permutations

perms :: Eq a => [a] -> [[a]]
perms [] = [[]]
perms xs = [ x:y | x <- xs, y <- perms (delete x xs) ]

--d

--sumandos n = 
