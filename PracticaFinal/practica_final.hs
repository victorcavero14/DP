-- Alumno: Víctor Manuel Cavero Gracia 45355080T
-- Asignatura: Programación Declarativa
-- Práctica Final

import System.IO -- Necesario para la segunda parte

-- Tipos de datos utilizados

data Vertice = A|B|C|D|E|F deriving (Show, Read, Eq)

data Grafo = G [Vertice] [(Vertice, Vertice)] deriving (Show, Read, Eq)

type Matriz = [[Bool]]

-- Variables globales tipo Grafo para las pruebas de los algoritmos (Al menos dos isomorfos entre si y al menos dos no) 

g1 =  G [B,D,E,C] [(D,E),(E,B),(C,B),(E,C)]
g2 =  G [D,F,E] [(D,F),(E,D),(D,E),(F,E)]
g3 =  G [A,C,D] [(A,C),(C,D),(A,D)]
g4 = 
g5 = 
g6 = 

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

camino_lng :: Grafo -> Vertice -> Int -> [[Vertice]]
camino_lng (G vs as) v n
   | not $ es_grafo (G vs as) = error "No es un grafo valido"
   | as == [] = []
   | n == 0 = [[v]]
   | n > length vs = []
   | otherwise = concat $ camino_lng_auxi (G vs as) 0 v n
   where
      camino_lng_auxi :: Grafo -> Int -> Vertice -> Int -> [[[Vertice]]]
      camino_lng_auxi (G vs as) i v n
         | i == length vs = []
         | auxiliar /= [[]] = auxiliar : camino_lng_auxi (G vs as) (i+1) v n
         | otherwise = camino_lng_auxi (G vs as) (i+1) v n
         where
            auxiliar = filter (\l -> length l == (n+1)) $ genera_caminos (buscar_adyacentes (G vs as)) v (vs !! i)

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

-- Dado un vertice y un grafo, obtiene los vertices adyacentes al vertice dado
buscar_adyacentes :: Grafo -> Vertice -> [Vertice]
buscar_adyacentes (G _ []) _ = []
buscar_adyacentes (G vs (a:as)) v 
   | (notElem v vs) = []
   | (fst a) == v = (snd a) : buscar_adyacentes (G vs as) v
   | otherwise = buscar_adyacentes (G vs as) v

-- Dada la funcion buscar_adyacente y dos vertices (uno de inicio y otro de final) te devuelve
-- una lista de los caminos posibles para llegar entre ellos
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

leegrafo 

leegrafo :: IO()
leegrafo = do
    vertice <- inserteVertice
   
inserteVertice :: IO([Vertice])
inserteVertice = do
    putStr "Insete un vertice del grafo: "
    vertice <- getLine
    if (vertice == "") then putStrLn "ERROR Vertice invalido"
    else
       if(vertice == "-1") then do
          putStrLn "Vertices escogidos"
          return vertice
       else do
          putStrLn "Vertice insertado"
          inserteVertice
          
--muestra_matriz
--muestra_caminos


