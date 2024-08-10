data List a = Nil | Cons a (List a) deriving (Show, Eq)



sumar :: List Int -> Int

sumar Nil                    = 0

sumar (Cons x xs) = x + sumar xs



data Expr = Literal Int | Suma Expr Expr | Resta Expr Expr deriving (Show, Eq)



ejercicio :: Expr

ejercicio = (Resta (Suma (Literal 1) (Literal 3)) (Literal 5))



mostrarExpr :: Expr -> String 

mostrarExpr (Literal n) = show n 

mostrarExpr (Suma e1 e2) = "(" ++ mostrarExpr e1 ++ " + " ++ mostrarExpr e2 ++ ")"

mostrarExpr (Resta e1 e2) = "(" ++ mostrarExpr e1 ++ " - " ++ mostrarExpr e2 ++ ")"



evaluar :: Expr -> Int

evaluar (Literal n) = n 

evaluar (Suma e1 e2) = evaluar e1 + evaluar e2

evaluar (Resta e1 e2) = evaluar e1 - evaluar e2



buscar :: Int -> Expr -> Bool

buscar m (Literal n) = m == n

buscar m (Suma e1 e2) = buscar m e1 || buscar m e2

buscar m (Resta e1 e2) = buscar m e1 || buscar m e2



list2alg :: [a] -> List a

list2alg [] = Nil

list2alg (x:xs) = (Cons x (list2alg xs))



divisionSegura :: Float -> Float -> Maybe Float

divisionSegura _ 0 = Nothing

divisionSegura  a b = Just (a/b)



join :: (a -> c) -> (b -> d ) -> Either a b -> Either c d

join f _ (Left x)  = Left (f x)

join _ g (Right y) = Right (g y)



type Resultado a = Either a String



llamarDivision :: Float -> Float -> Resultado Float

llamarDivision x y = case divisionSegura x y of

    Just r  -> Left r

    Nothing -> Right "División por cero"