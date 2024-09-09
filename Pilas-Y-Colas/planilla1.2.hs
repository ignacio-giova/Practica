data Nat = Zero | Succ Nat



foldNat :: a -> (a -> a) -> Nat -> a

foldNat z _ Zero = z 

foldNat z f (Succ n) = f (foldNat z f n)



data List a = Nil | Cons a (List a) deriving (Show)



foldList :: b -> (a -> b -> b ) -> List a -> b

foldList z _ Nil = z 

foldList z f (Cons x xs) = f x (foldList z f xs)



nat2int' :: Nat -> Int

nat2int' = foldNat 0 (+1)



sumars :: List Int -> Int

sumars = foldList 0 (+)



list3alg :: [a] -> List a 

list3alg = foldr Cons Nil



data Expr = Literal Int | Suma Expr Expr | Resta Expr Expr



foldExpr :: (Int -> r) -> (r -> r-> r) -> (r -> r -> r) -> Expr -> r

foldExpr f _ _ (Literal n) = f n 

foldExpr f g h (Suma x y) = g (foldExpr f g h x) (foldExpr f g h y)

foldExpr f g h (Resta x y) = h (foldExpr f g h x) (foldExpr f g h y)



mostrarS :: Expr -> String

mostrarS = foldExpr show (\x y -> "(" ++ x ++ "+" ++ y ++ ")") (\x y -> "(" ++ x ++ "-" ++ y ++ ")")



evaluarS :: Expr -> Int

evaluarS = foldExpr id (+) (-)



repeat' :: a -> Nat -> List a

repeat' x = foldNat Nil (\xs -> Cons x xs)



mapS :: List a -> (a -> b) -> List b 

mapS a f = foldList Nil (\x xs -> Cons (f x) xs) a



filter' :: ( a -> Bool) -> List a -> List a 

filter' f = foldList Nil (\x xs -> if (f x) then Cons x xs else xs)



data Traza = Izquierda Traza | Derecha Traza | Adelante Traza | Nada deriving (Show)



foldTraza :: r -> (r -> r) -> (r -> r) -> (r -> r) -> Traza -> r

foldTraza z _ _ _ Nada          = z

foldTraza z f g h (Izquierda t) = f (foldTraza z f g h t)

foldTraza z f g h (Derecha t)   = g (foldTraza z f g h t)

foldTraza z f g h (Adelante t)  = h (foldTraza z f g h t)



trazaEjemplo = Adelante (Izquierda (Derecha Nada))



cantidadDeVecesQueFuimosALaIzquierda :: Traza -> Int

cantidadDeVecesQueFuimosALaIzquierda = foldTraza 0 (+1) id id