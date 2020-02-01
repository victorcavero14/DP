
-- Ejercicio 1: 

years :: Floating a => a
years = (10^10) / (60*60*24*365)

years_2 :: (Integral a) => [a]
years_2 = let aux = years - (fromIntegral (round years)) in
   map round [years, aux*365, aux*365*24, aux*365*24*60, aux*365*24*60*60] 

years_3 :: (Ord a, Fractional a) => a -> a
years_3 secs
    | secs < 0 = 0
    | otherwise = secs / (60*60*24*365)

years_4 :: (RealFrac a , Integral b) => a -> [b]
years_4 secs
    | secs < 0 = []
    | otherwise = map round [years_3 secs, aux*365, aux*365*24, aux*365*24*60, aux*365*24*60*60] 
    where aux = years_3 secs - (fromIntegral $ round $ years_3 secs)


-- Ejercicio 3: 

media :: (Fractional b) => [b] -> b
media xs = sum xs / (fromIntegral $ length xs)

-- Ejercicio 4:

digitos :: (Integral a) => a -> a
digitos 0 = 0
digitos x = 1 + digitos (div x 10) 

reduccion :: (Integral a) => a -> a
reduccion x  
    | x < 0 = reduccion (abs (x))
    | (sumadigitos x < 10) && (sumadigitos x >= 0) = x
    | otherwise = reduccion $ sumadigitos x

--auxiliar de la anterior
sumadigitos :: (Integral a) => a -> a
sumadigitos x
    | x == 0 = 0
    | otherwise = (mod x 10) + sumadigitos (div x 10)

