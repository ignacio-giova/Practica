import Data.Maybe
import Data.Map
import Prelude hiding (lookup)

data Contacto = CDatos String String String String
type Tel = Int
type Agenda = [(Tel, Contacto)]

-- Función para buscar una clave
buscar :: Eq a => a -> [(a, [b])] -> Maybe [b]
buscar _ [] = Nothing
buscar k ((k', vs):xs)
  | k == k' = Just vs
  | otherwise = buscar k xs

-- Función para borrar una clave
borrar :: Eq a => a -> [(a, [b])] -> [(a, [b])]
borrar _ [] = []
borrar k ((k', vs):xs)
  | k == k' = xs
  | otherwise = (k', vs) : borrar k xs

-- Función para agregar una clave
agregar :: Eq a => a -> [b] -> [(a, [b])] -> [(a, [b])]
agregar k vs resultado = (k, vs) : resultado

-- Acumulación de claves
acumulacionClaves :: Eq a => [(a, b)] -> [(a, [b])]
acumulacionClaves xs = go xs []
  where
    go [] resultado = resultado
    go ((k, v):xs) resultado
      | isJust (buscar k resultado) =
        let vs = fromJust (buscar k resultado) in
        go xs (agregar k (v:vs) (borrar k resultado))
      | otherwise = go xs ((k, [v]) : resultado)

acumulacionClaves2 :: Ord a => [(a, b)] -> Map a [b]
acumulacionClaves2 xs = go xs empty
  where
    go [] resultado = resultado
    go ((k, v):xs) resultado
      | isJust (lookup k resultado) =
        let vs = fromJust (lookup k resultado) in
        go xs (insert k (v:vs) resultado)
      | otherwise = go xs (insert k [v] resultado)

memtriang :: Int -> Map Int Int
memtriang n = go 1 (insert 0 0 empty)
  where
    go i resultado  
      | n == i = resultado
      | otherwise =
        let nTriangular = i + resultado ! (i-1) in
        go (i+1) (insert i nTriangular resultado)  
    

numeroTraingular :: Int -> Int
numeroTraingular 0  = 0
numeroTraingular n = n + (numeroTraingular (n-1) )

cache :: Map Int Int
cache = memtriang 100000

fasttriang :: Int -> Int
fasttriang = (cache !)

memtriangSlow :: Int -> [(Int, Int)]
memtriangSlow n = reverse (go 1 [(0,0)])
  where
    go i resultado
      | n + 1== i = resultado
      | otherwise =
        let new = i + snd (head resultado) in
        go (i+1) ((i, new) : resultado)

cacheSlow :: [(Int, Int)]
cacheSlow = memtriangSlow 10000

fasttriangSlow :: Int -> Int
fasttriangSlow n = buscar cacheSlow
  where
    buscar [] = error "No se encontro"
    buscar ((k , v): xs)
      |k == n = v
      |otherwise = buscar xs
