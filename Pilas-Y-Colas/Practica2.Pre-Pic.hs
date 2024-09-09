data Pila a = PEmpty | PTop a (Pila a)

instance Stack Pila a where
    empty :: Pila a 
    empty = PEmpty

    push :: a -> Pila a -> Pila a
    push m stk = PTop m stk

    top :: Pila a -> a
    top (PTop m stk) = m

    pop :: Pila a -> Pila a
    pop (PTop m stk) = stk

    isEmpty :: Pila a -> Bool
    isEmpty PEmpty = True
    isEmpty _ = False

data FastQueue a = FQ ([a], [a]) deriving (Show, Eq)

instance Stack FastQueue a where
    empty :: FastQueue a 
    empty = FQ ([a], [a])

    enqueue :: a -> FastQueue a -> FastQueue a
    enqueue m (FQ (inis, outs)) = FQ (m:inis, outs)

    front :: FastQueue a -> a
    front (FQ (inis, outs)) = head outs

    dequeue :: FastQueue a -> FastQueue a
    dequeue (FQ (ins, [])) = dequeue (FQ ([], reverse ins))
    dequeue (FQ (ins, outs)) = FQ (ins, tail outs)

    isEmpty :: FastQueue a -> Bool
    isEmpty (FQ ([], [])) = True
    isEmpty _ = False

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)

insertar :: a -> Tree a -> Tree a
insertar m Empty = Node m Empty Empty
insertar m (Node x l r) = Node x (insertar m l) r

contarNodos :: Tree a -> Int
contarNodos Empty = 0
contar (Node x r l) = 1 + (contarNodos l) + (contarNodos r)

altura :: Tree a -> Int
altura Empty = 0
altura (Node x r l) = 1 + max ((altura l), (altura r))

foldArbol :: b -> (a -> b -> b -> b) -> Tree a -> b 
foldArbol z _ Empty = z
foldArbol z f (Node x l r) = f x (foldArbol z f l) (foldArbol z f r)

count' :: Tree a -> Int
count' m xs = foldArbol 0 (\x y z -> 1 + y + z) xs

height' :: Tree a -> Int
height' m xs = foldArbol 0 (\x y z -> 1 + max (y,z)) xs

