import Data.List

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

