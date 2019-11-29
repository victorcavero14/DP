--Víctor Manuel Cavero Gracia, 3ºA

-- 1
    calcularCuadradosNat :: Num a => [a] -> [a]
    calcularCuadradosNat [] = []
    calcularCuadradosNat (x:xs) = (x^2) : (calcularCuadradosNat xs)


    calcularCuadradosTup :: Num a => [a] -> [(a,a)]
    calcularCuadradosTup [] = []
    calcularCuadradosTup (x:xs) = (x,(x^2)) : (calcularCuadradosTup xs)


    calcularSumatorio :: Double
    calcularSumatorio = calcularSumatorioAux 1
    calcularSumatorioAux i
        | i > 100 = 0
        | otherwise = (i * abs(sin (i))) + calcularSumatorioAux (i+1)



    calcularPotn_67 :: Integral a => a -> [a]
    calcularPotn_67 n = calcularPot3 n 0

    calcularPot3 n x
        | ((pot < n) && ((pot `mod` 100) == 67)) = pot : calcularPot3 n (x+1)
        | (pot < n) = calcularPot3 n (x+1)
        | otherwise = []
        where pot = 3^x


    sumaNums :: Integer
    sumaNums = sumaNumsAux 0

    sumaNumsAux i 
        | i > 1000 = 0
        | (i `mod` 3 == 0) || (i `mod` 5 == 0) = i + sumaNumsAux(i+1)
        | otherwise = sumaNumsAux(i+1)


-- 2.

    filter2 :: [a] -> (a -> Bool) -> (a -> Bool) -> ([a], [a])
    filter2 xs p q = (filter p xs , filter q xs)


    --filters _ [] = []
    --filters [] _ = []
    --filters (x:xs) (p:ps) = (filter p x) : filters xs ps


    mapx _ [] = []
    mapx [] _ = []
    mapx x (f:fs) = (map f x) : (mapx x fs)


    --iguales f g n m = iguales_aux f g n m n

    --iguales_aux f g n m i 
    --    | (f i == g i) = (True && iguales_aux (f g n m (i+1)))
    --    | otherwise = False


    cuantos _ [] = 0
    cuantos p (x:xs)
        | p x = 1 + cuantos p xs
        | otherwise = 0 + cuantos p xs


    -- menorA n m p


    -- mayor n p


    -- ex n m p

-- 3.

    last' :: Num a => [a] -> a
    last' [] = 0
    last' xs = foldl (\x y -> y) 1 xs

    reverse' :: Foldable t => t a -> [a]
    reverse' xs = foldr (\x y -> y ++ [x]) [] xs

    all' :: Foldable t => (a -> Bool) -> t a -> Bool
    all' p xs = foldl (\x y -> x && (p y) ) True xs

    minimum' :: (Foldable t, Ord a) => t a -> a
    minimum' xs = foldr (\x y -> min x y) (maximum xs) xs

    map' :: Foldable t => (t1 -> a) -> t t1 -> [a]
    map' f xs = foldr (\x y -> f x : y) [] xs

    filter' :: Foldable t => (a -> Bool) -> t a -> [a]
    filter' p xs = foldr (\x y -> if p x then x : y else y) [] xs

    takeWhile' :: Foldable t => (a -> Bool) -> t a -> [a]
    takeWhile' p xs = foldr (\x y -> if p x then x : y else []) [] xs

    -- La diferencia entre takeWhile y filter es que
    -- takeWhile para de buscar cuando encuentra un elemento que no cumple
    -- la propiedad p 

    masmas :: [a] -> [a] -> [a]
    masmas xs ys = foldr (\x y -> x:y) ys xs

-- 4.

    --foldr1 f xs = foldr f 0 xs 


    --foldl1 f xs = 

-- 5.

