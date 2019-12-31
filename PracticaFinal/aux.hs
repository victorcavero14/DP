buscar_aristas_comienzo_total :: [(Vertice,Vertice)] -> [Vertice] -> Int -> Vertice -> Int -> [[(Vertice,Vertice)]]
buscar_aristas_comienzo_total as (vi:vs) i v n
   | (i == n) = []
   | otherwise = (z) : (buscar_aristas_comienzo_total as (filter (/= v) (map (snd) z)) (i+1) v n)
   where 
      z = concat $ buscar_aristas_comienzo_aux as (vi:vs) (length (vi:vs)) 0
      buscar_aristas_comienzo_aux :: [(Vertice,Vertice)] -> [Vertice] -> Int -> Int -> [[(Vertice,Vertice)]]
      buscar_aristas_comienzo_aux az (vz:vzz) l j
         | (az == []) = []
         | ((vz:vzz) == []) = []
         | (j == l) = []
         | otherwise = (buscar_aristas_comienzo az vz) : buscar_aristas_comienzo_aux az vzz l (j+1)


camino_lng_aux :: [Vertice] -> [(Vertice, Vertice)] -> Vertice -> Int -> [[(Vertice,Vertice)]]
camino_lng_aux vs as v n 
   | (notElem v vs ) = []
   | (n == 0) = []
   | (as == []) = []
   | otherwise = buscar_aristas_comienzo_total as (v:[]) 0 v n

--ordena_vertices :: [Vertice] -> [Vertice]
--ordena_vertices xs = ordena_vertices_aux ([minBound .. maxBound] :: [Vertice]) xs 0
 --  where
 --     ordena_vertices_aux :: [Vertice] -> [Vertice] -> Int -> [Vertice]
 --     ordena_vertices_aux _ [] _ = []
 --     ordena_vertices_aux xs ys i 
 --        | (i == length xs) = []
 --        | (elem (xs !! i) ys) = (xs !! i) : ordena_vertices_aux xs ys (i+1)
 --        | otherwise = ordena_vertices_aux xs ys (i+1)
