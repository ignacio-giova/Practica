data Forma = Circulo Float | Rectangulo Float Float | Triangulo Float Float Float

perimetro :: Forma -> Float
perimetro (Circulo r) = 3.14 * r * 2
perimetro (Rectangulo x y) = (x + y) * 2
perimetro (Triangulo x y z) = x + y + z

data Expr = Literal Int | Suma Expr Expr | Resta Expr Expr deriving (Show, Eq)

evaluar :: Expr -> Int
evaluar (Literal x) = x
evaluar (Suma e1 e2) = evaluar e1 + evaluar e2
evaluar (Resta e1 e2) = evaluar e1 - evaluar e2

mostrar :: Expr -> String
mostrar (Literal x) = show x 
mostrar (Suma e1 e2) = "(" ++ (mostrar e1) ++ "+" ++ (mostrar e2) ++ ")"
mostrar (Resta e1 e2) = "(" ++ (mostrar e1) ++ "-" ++ (mostrar e2) ++ ")"

buscar :: Int -> Expr -> Bool
buscar m (Literal x) = m == x
buscar m (Suma e1 e2) =  (buscar m e1) || (buscar m e2) 
buscar m (Resta e1 e2) = (buscar m e1) || (buscar m e2) 

data List a = Nil | Cons a (List a) deriving (Show, Eq)

list2alg :: [a] -> List a
list2alg [] = Nil
list2alg (x:xs) = Cons x (list2alg xs)

data Nat = Zero | Succ Nat

foldNat :: a -> (a -> a) -> Nat -> a
foldNat z _ Zero = z
foldNat z f (Succ x) = f (foldNat z f x)

foldExpr :: (Int -> r) -> (r -> r-> r) -> (r -> r -> r) -> Expr -> r
foldExpr f _ _ (Literal n) = f n 
foldExpr f g h (Suma x y) = g (foldExpr f g h x) (foldExpr f g h y)
foldExpr f g h (Resta x y) = h (foldExpr f g h x) (foldExpr f g h y)

foldList :: b -> (a -> b -> b) -> List a -> b
foldList z _ Nil = z 
foldList z f (Cons x xs) = f x (foldList z f xs)

contar :: Nat -> Int
contar = foldNat 0 (\x -> 1 + x)

agregar :: a -> List a -> List a 
agregar x xs = (Cons x xs)

buscarM :: Eq a => a -> List a -> Bool
buscarM m xs = foldList False (\x y -> m == x || y) xs 

quitar :: Eq a => a -> List a -> List a 
quitar m xs = foldList Nil (\x y -> if m /= x then (Cons x y) else y ) xs

cantidad :: List a -> Int
cantidad = foldList 0 (\x y -> 1 + y)

sumaS :: Expr -> Int
sumaS = foldExpr (\x -> id x) (\x y -> x + y ) (\x y -> x - y )

repetirElementos :: List a -> List a 
repetirElementos = foldList Nil (\x y -> (Cons x (Cons x y)))

--Prueba
data OpEd = Escribir Char | Borrar | Remplazar Char

aplicar :: OpEd -> String -> String
aplicar (Escribir x) xs = xs ++ [x]
aplicar Borrar xs = init xs
aplicar (Remplazar x) xs = aplicar (Escribir x)  (aplicar Borrar xs)