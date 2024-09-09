data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)



insertarElemento :: a -> Tree a -> Tree a 

insertarElemento x Empty = Node x Empty Empty

insertarElemento x (Node y e1 e2) = Node y (insertarElemento x e1) e2



contarNodos :: Tree a -> Int

contarNodos Empty = 0

contarNodos (Node x l r) = 1 + (contarNodso l) + (contarNodso r)



altura :: Tree a -> Int

altura Empty = 0

altura (Node y e1 e2) = 1 + max (altura e1) (altura e2)



aplicar :: (a -> b) -> Tree a -> Tree a 

aplicar Empty = Empty

aplicar (Node x l r) = (Node (f x) (aplicar f l) (aplicar f r))



foldArbol :: (a -> b -> b -> b) -> b -> Tree a -> b 

foldArbol _ z Empty = z

foldArbol f z (Node x l r) = f x (foldArbol f z l) (foldArbol f z r)



contarNodos' :: Tree a -> Int

contarNodos' = (\x y z -> 1 + y + z) 0 



altura' :: Tree a -> Int

altura' = (\x y z -> 1 + (max y z)) 0 



aplicar' :: (a->b) -> Tree a -> Tree a 

aplicar' = (\x y z -> Node (f x) y z) Empty



-- Hacer ejercicios Dificiles



type Heap a = Tree a


minElement :: Heap a -> a

minElement (Node x _ _ ) = x 



size :: Tree a -> Int

size = contarNodos



insert :: Ord a => a -> Heap a -> Heap a

insert x Empty = Node x Empty Empty

insert x (Node y l r)

  | x  < y && size l <= size r = Node x (insert y l) r

  | x  < y && size l  > size r = Node x l (insert y r)

  | x >= y && size l <= size r = Node y (insert x l) r

  | x >= y && size l  > size r = Node y l (insert x r)



deleteMin :: Ord a => Heap a -> Heap a

deleteMin Empty = Empty

deleteMin (Node _ Empty r) = r

deleteMin (Node _ l Empty) = l

deleteMin (Node d (Node x ll lr) (Node y rl rr))

  | x <= y    = Node x (deleteMin (Node d ll lr)) (Node y rl rr)

  | otherwise = Node y (Node x ll lr) (deleteMin (Node d rl rr))



instance PriorityQueue Tree where

    pqempty :: Ord a => Heap a 

    pqempty = Empty



    pqenqueue :: Ord a =>  a -> Heap a -> Heap a

    pqenqueue x stk = insert x stk



    front :: Ord a => Heap a -> a 

    front = minElement



    pqdequeue :: Ord a => Heap a -> Heap a 

    pqdequeue :: deleteMin 



    pqisEmpty :: Ord a => Heap a -> Bool

    pqisEmpty Empty = True

    pqisEmpty _ = False



toHeap :: Ord a => [a] -> Heap a

toHeap = foldr insert Empty --Ver



fromHeap :: Ord a => Heap a -> [a]

fromHeap Empty = []

fromHeap h = minElement h : fromHeap (deleteMin h)



data GenTree a = Node a [GenTree a] deriving (Show, Eq)



aplanar :: GenTree a -> [a]

aplanar (Node []) = []

aplanar (Node x:xs) = 