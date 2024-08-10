data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)

node :: Tree a -> a
node (Node x _ _) = x

insertLeft :: a -> Tree a -> Tree a
insertLeft x Empty = Node x Empty Empty
insertLeft x (Node y l r) = Node y (insertLeft x l) r

countNodes :: Tree a -> Int
countNodes Empty = 0
countNodes (Node _ l r) = 1 + countNodes l + countNodes r

height :: Tree a -> Int
height Empty = 0
height (Node _ l r) = 1 + max (height l) (height r)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Empty = Empty
mapTree f (Node x l r) = Node (f x) (mapTree f l) (mapTree f r)

foldTree :: (a -> b -> b -> b) -> b -> Tree a -> b
foldTree _ z Empty = z
foldTree f z (Node x l r) = f x (foldTree f z l) (foldTree f z r)

countNodes' :: Tree a -> Int
countNodes' = foldTree (\_ l r -> 1 + l + r) 0

height' :: Tree a -> Int
height' = foldTree (\_ l r -> 1 + max l r) 0

mapTree' :: (a -> b) -> Tree a -> Tree b
mapTree' f = foldTree (\x l r -> Node (f x) l r) Empty

data GenTree a = GNode a [GenTree a] deriving (Show, Eq)

gnode :: GenTree a -> a
gnode (GNode x _) = x

gchildren :: GenTree a -> [GenTree a]
gchildren (GNode _ ts) = ts

flattenList :: [[a]] -> [a]
flattenList xss = foldl (++) [] xss

flattenTree :: GenTree a -> [a]
flattenTree gt = flattenTree' [gt]

flattenTree' :: [GenTree a] -> [a]
flattenTree' [] = []
flattenTree' ts = map gnode ts ++ flattenTree' (flattenList (map gchildren ts))

-- no lo hace de a pares exáctamente
-- pero es más fácil que rellenar con ceros
-- hasta la potencia de dos más cercana
comprimir :: [Tree Int] -> [Tree Int]
comprimir [] = []
comprimir [t] = [t]
comprimir (t1:t2:ts) = Node (node t1 + node t2) t1 t2 : comprimir ts

sumasIntermedias :: [Int] -> Tree Int
sumasIntermedias [] = Empty
sumasIntermedias xs = head (sumasIntermedias' (map (\x -> Node x Empty Empty) xs))

sumasIntermedias' :: [Tree Int] -> [Tree Int]
sumasIntermedias' [] = []
sumasIntermedias' [t] = [t]
sumasIntermedias' ts = sumasIntermedias' (comprimir ts)

type Heap a = Tree a

minElement :: Heap a -> a
minElement = node

size :: Tree a -> Int
size = countNodes

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

  pqenqueue :: Ord a => a -> Heap a -> Heap a
  pqenqueue = insert

  pqfront :: Ord a => Heap a -> a
  pqfront = minElement

  pqdequeue :: Ord a => Heap a -> Heap a
  pqdequeue = deleteMin

  pqisEmpty :: Ord a => Heap a -> Bool
  pqisEmpty Empty = True
  pqisEmpty _ = False

toHeap :: Ord a => [a] -> Heap a
toHeap = foldr insert Empty

fromHeap :: Ord a => Heap a -> [a]
fromHeap Empty = []
fromHeap h = minElement h : fromHeap (deleteMin h)

heapsort :: Ord a => [a] -> [a]
heapsort = fromHeap . toHeap

left :: Tree a -> Tree a
left (Node _ l _) = l

right :: Tree a -> Tree a
right (Node _ _ r) = r

-- se rompe muy poquito, pero la idea es que
-- si nos aseguramos de insertar los elementos más chicos
-- de un mismo lado del árbol, al sacarlos todos
-- el árbol se desbalancea
ejemploQueRompe :: (Heap Int, Int, Int)
ejemploQueRompe =
  let one      = insert 1 pqempty
      two      = insert (-2) one
      three    = insert 3 two
      four     = insert (-4) three
      five     = insert 5 four
      six      = insert 6 five
      seven    = insert 7 six
      eight    = insert (-8) seven
      nine     = insert 9 eight
      ten      = insert 10 nine
      eleven   = insert 11 ten
      twelve   = insert 12 eleven
      thirteen = insert 13 twelve
      fourteen = insert 14 thirteen
      fifteen  = insert 15 fourteen
      sixteen  = insert (-16) fifteen

      deleted = deleteMin (deleteMin (deleteMin (deleteMin sixteen)))
  in (deleted, size (left deleted), size (right deleted))

newtype Int' = Int' Int deriving (Eq, Show)

instance Ord Int' where
  (<=) :: Int' -> Int' -> Bool
  Int' x <= Int' y = y <= x

heapsortinv :: [Int] -> [Int]
heapsortinv ints = map (\(Int' x) -> x) (heapsort (map Int' ints))

type Heap a = Tree a

size :: Tree a -> Int
size = countNodes
 
insert :: Ord a => a -> Heap a -> Heap a 
insert x Empty = Node x Empty Empty
insert x (Node y l r) =
    | x < y && size l < size r = Node x (insert y l ) r
    | x < y && size l > size r = Node x l (insert y r )
    | x > y && size l < size r = Node y (insert x l ) r
    | x > y && size l > size r = Node y l (insert x r )

deleteMin :: Ord a => Heap a -> Heap a 
deleteMin Empty = Empty
deleteMin (Node _ l Empty) = l
deleteMin (Node _ Empty r) = r
deleteMin (Node d (Node x ll lr) (Node y rl rr)) = 
    |x <= y = 
        Node x (deleteMin (Node d ll lr)) (Node y rl rr)
    | otherwise = 
        Node y (Node x ll lr) (deleteMin (Node d rl rr))