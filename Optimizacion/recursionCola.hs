sumArray :: [Int] -> Int
sumArray arr = go arr 0
  where
    go [] acc = acc
    go (x:xs) acc = go xs (acc + x)

factorial :: Int -> Int
factorial n = go n 1
  where
    go 0 acc = acc  
    go x acc = go (x-1) (x * acc)

--Revisar
fibonacci :: Int -> Int
fibonacci n = go n 0 1
  where
    go 0 a _ = a
    go n a b = go (n-1) b (a + b)

collatz :: Int ->  Int
collatz n = go n 
  where 
    go 1 = 1
    go n = if even n
              then go (n `div` 2)
              else go (n * 3 + 1)