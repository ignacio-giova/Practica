import Data.List
import Data.Map
import Prelude hiding (lookup)

ordenar :: Ord a => [a] -> [a]
ordenar = Data.List.sort

selectionsort [] = []
selectionsort l =
    let m = minimum l in
    let r = delete m l in
    m : selectionsort r

numerosDistintos :: [Int] -> Int
numerosDistintos xs = 
  let sl = selectionsort xs in
  length(go sl)
    where
      go [] = []
      go [x] = [x]
      go (x:xs) 
        |x == head xs = go xs 
        |otherwise = x : go xs

--Lista de Pesos, p como peso max por gondola
ruedaFortuna :: [Int] -> Int -> Int
ruedaFortuna xs p = go (ordenar xs) p 0
    where
        go [] _ resultado = resultado
        go [x] _ resultado = resultado + 1
        go xs p resultado 
          |((head xs) + (last xs)) > p = go (init xs) p (resultado + 1)
          |otherwise = go (tail(init xs)) p (resultado + 1)

instance Ord Accion where
  compare (Entrada t1) (Entrada t2) = compare t1 t2
  compare (Salida t1) (Salida t2) = compare t1 t2
  compare (Entrada t1) (Salida t2) = compare t1 t2
  compare (Salida t1) (Entrada t2) = compare t1 t2

data Accion = Entrada Int | Salida Int deriving (Show, Eq)

maxComensales :: [(Int, Int)] -> Int
maxComensales comensales = 
  let acciones = aplanarAcciones comensales in
  maximum (go (ordenar acciones) 0)
  where 
    aplanarAcciones :: [(Int, Int)] -> [Accion]
    aplanarAcciones = foldr (\(e, s) r -> Entrada e : Salida s : r) []

    go [] actuales = [actuales]
    go ((Entrada _): xs) actuales = actuales : go xs (actuales + 1)
    go ((Salida _): xs) actuales = actuales : go xs (actuales - 1)

dosValores :: [Int] -> Int -> Maybe (Int, Int)
dosValores lista n = go lista (acumular lista empty)
  where
    acumular :: [Int] -> Map Int Int -> Map Int
    acumular [] r = r
    acumular (x:xs) map=
      case lookup x map of
        Nothing -> acumular xs (insert x 1 map)
        Just c -> acumular xs (insert x (c + 1) (delete c map))
    
    go [] _ = Nothing
    go (x:xs) map =
      let comp = n - x 
      case lookup comp map of
        Nothing -> go xs map
        Just veces ->
          if comp == n && veces == 1
            then go xs map
            else (x, comp)
