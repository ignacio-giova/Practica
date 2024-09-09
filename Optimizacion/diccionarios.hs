import Data.Maybe

data Contacto = CDatos String String String String
type Tel = Int
type Agenda = [(Tel, Contacto)]

buscar t [] = Nothing
buscar t ((t', c):xs)
  | t == t' = Just c
  | otherwise = buscar t xs

agregar t c xs = (t, c): xs

borrar t [] = []
borrar t ((t', c): xs)
  | t == t' = xs
  | otherwise = (t', c) : (borrar t xs)

acumulacionClaves:: Eq a => [(a,b)] -> [(a, [b])]
acumulacionClaves xs = go xs []
  where
    go [] resultado = resultado
    go ((k, v):xs) resultado
      | isJust (buscar k resultado) =
        let (Just vs) = buscar k resultado in
        let resultadoBorrado = borrar k resultado in
        agregar k (v:vs) resultadoBorrado
      | otherwise = go xs ((k, [v]) : resultado)
