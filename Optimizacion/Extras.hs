import Data.List

ordenar :: Ord a => [a] -> [a]
ordenar = Data.List.sort


numeroFaltante :: [Int] -> Maybe Int
numeroFaltante xs = go (ordenar xs)
  where
    go [] = Nothing
    go [x] = Nothing
    go (x1:x2:xs)
      | (x1 + 1) /= x2 = Just (x1 + 1)
      | otherwise = go (x2:xs)

mayorDiferencia :: [Int] -> (Int, Int)
mayorDiferencia xs = go (ordenar xs) (0, 0)
    where 
        go [] resultado = resultado
        go [x] resultado = resultado
        go (x1:x2:xs) resultado
          | (x2 - x1) > (diferencia resultado) = go (x2:xs) (x1, x2)
          | otherwise = go (x2:xs) resultado

        diferencia :: (Int ,Int) -> Int
        diferencia (a, b) = b - a

tiempoMin :: [Int] -> Int
tiempoMin xs = go (reverse $ ordenar xs) 0 0
  where
    go [] a b = a + b + abs(a-b)
    go (x:xs) a b 
      | a < b = go xs (a + x) b 
      | otherwise = go xs a (b + x) 
