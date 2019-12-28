-- Alumno: Víctor Manuel Cavero Gracia
-- Asignatura: Programación Declarativa
-- Práctica Final

data Vertice = A|B|C|D|E|F deriving (Show, Read, Eq)

data Grafo = G [Vertice] [(Vertice, Vertice)] deriving (Show, Read, Eq)

type Matriz = [[Bool]]

-- PRIMERA PARTE

es_grafo :: Grafo -> Bool
es_grafo (G [] _) = False
es_grafo (G vs as) = (not $ repetidos vs) && aristas_correctas (G vs as)
   where
      aristas_correctas :: Grafo -> Bool
      aristas_correctas (G _ [] ) = True
      aristas_correctas (G vs ((y1,y2):as) )
         | elem y1 vs && elem y2 vs && notElem (y1,y2) as = aristas_correctas (G vs as)
         | otherwise = False 

      repetidos :: [Vertice] -> Bool
      repetidos [] = False
      repetidos (x:xs) 
         | filter (==x) xs == [] = repetidos xs
         | otherwise = True
 
-- Matriz de adyacencia en la que no considero el orden de los vertices mediante el alfabeto (A,B,C...)
-- si no de la manera que me lo dan en el grafo constructor.

mat_ady :: Grafo -> Matriz
mat_ady (G vs as) 
   | not $ es_grafo (G vs as) = error "No es un grafo valido"
   | otherwise = crea_sub_matriz (G vs as) 0
   where 
      crea_sub_matriz :: Grafo -> Int -> Matriz
      crea_sub_matriz (G vs as) i
         | i == length vs = []
         | otherwise = crea_sub_matriz_lista (G vs as) i 0 : crea_sub_matriz (G vs as) (i+1)

      crea_sub_matriz_lista :: Grafo -> Int -> Int -> [Bool]
      crea_sub_matriz_lista (G vs as) i j
         | j == length vs = []
         | elem (vs !! i, vs !! j) as  =  True : crea_sub_matriz_lista (G vs as) i (j+1)
         | otherwise = False : crea_sub_matriz_lista (G vs as) i (j+1)


grados_pos :: Grafo -> [(Vertice,Int)]
grados_pos g 
   | not $ es_grafo g = error "No es un grafo valido"
   | otherwise = grados_pos_aux g (mat_ady g) 0 --Podría hacerse con un zip pero es menos eficiente
   where 
      grados_pos_aux :: Grafo -> Matriz -> Int -> [(Vertice, Int)]
      grados_pos_aux (G vs as) ms i
         | i == length vs = []
         | otherwise = (vs !! i, sum $ map fromEnum (ms !! i)) : grados_pos_aux (G vs as) ms (i+1)


grados_neg :: Grafo -> [(Vertice, Int)]
grados_neg g 
   | not $ es_grafo g = error "No es un grafo valido"
   | otherwise = grados_neg_aux g (mat_ady g) 0
      where 
         grados_neg_aux :: Grafo -> Matriz -> Int -> [(Vertice, Int)]
         grados_neg_aux (G _ _) [[]] _ = []
         grados_neg_aux (G vs as) ms i
           | i == length ms = []
           | otherwise = (vs !! i, sum $ map fromEnum $ map (!! i) ms ) : grados_neg_aux (G vs as) ms (i+1)

--camino_lng :: Grafo -> Vertice -> Int -> [[(Vertice, Vertice)]]
--camino_lng (G vs as) v n = camino_lng_aux vs as v n

conexo :: Grafo -> Bool
conexo (G vs as)
   | not $ es_grafo (G vs as) = error "No es un grafo valido"
   | as == [] = False
   | otherwise = conexo_auxi (G vs as) 0
   where
      conexo_auxi :: Grafo -> Int -> Bool
      conexo_auxi (G vs as) i
         | i == length vs = True
         | otherwise = conexo_auxj (G vs as) i 0 && conexo_auxi (G vs as) (i+1)
      conexo_auxj :: Grafo -> Int -> Int -> Bool
      conexo_auxj (G vs as) i j
         | j == length vs = True
         | i == j = True && conexo_auxj (G vs as) i (j+1)
         | genera_caminos (buscar_adyacentes (G vs as)) (vs !! i) (vs !! j) /= [] = True && conexo_auxj (G vs as) i (j+1)
         | otherwise = False

-- FUNCIONES AUXILIARES

-- Dado un vertices y grafo, obtiene los vertices adyacentes al vertice dado
buscar_adyacentes :: Grafo -> Vertice -> [Vertice]
buscar_adyacentes (G _ []) _ = []
buscar_adyacentes (G vs (a:as)) v 
   | (notElem v vs) = []
   | (fst a) == v = (snd a) : buscar_adyacentes (G vs as) v
   | otherwise = buscar_adyacentes (G vs as) v

genera_caminos :: (Vertice -> [Vertice]) -> Vertice -> Vertice -> [[Vertice]]
genera_caminos succ prin fin = map reverse $ busca [] [[]] prin
  where
    busca :: [Vertice] -> [[Vertice]] -> Vertice -> [[Vertice]]
    busca vis actuales sig
      | sig == fin = avanza sig actuales
      | elem sig vis = []
      | otherwise = concat . map (busca (sig:vis) (avanza sig actuales)) $ succ sig

    avanza :: Vertice -> [[Vertice]] -> [[Vertice]]
    avanza sig = map ((:) sig)


-- SEGUNDA PARTE

--leegrafo
--muestra_matriz
--muestra_caminos

--Poner todos los tipos de las funciones, solo usar PRELUDE y definir 6 grafos ejemplos (3 dados y 3 mios)

obtener_g1 :: Grafo
obtener_g1 = G [B,D,E,C] [(D,E),(E,B),(C,B),(E,C)]

obtener_g2 :: Grafo
obtener_g2 = G [D,F,E] [(D,F),(E,D),(D,E),(F,E)]

obtener_g3 :: Grafo
obtener_g3 = G [A,C,D] [(A,C),(C,D),(A,D)]
