import Data.Maybe
import Data.Map

data Contacto = CDatos String String String String

type Tel = Int

type Agenda = [(Tel, Contacto)]

buscarContacto :: Tel -> Agenda -> Maybe Contacto
buscarContacto telefono [] = Nothing
buscarContacto telefono ((t, c): agenda)
  | t == telefono = Just c
  | otherwise = buscarContacto telefono agenda

agregarContacto :: Tel -> Contacto -> Agenda -> Agenda
agregarContacto telefono contacto agenda
  | isNothing (buscarContacto telefono agenda) = (telefono, contacto) : agenda
  | otherwise = agenda

borrarContacto :: Tel -> Agenda -> Agenda
borrarContacto telefono ((t, c): agenda) =
  if t == telefono
    then agenda
    else (t, c) : borrarContacto telefono agenda

acumularClaves :: Eq a => [(a, b)] -> [(a, [b])]
acumularClaves xs = go xs []
  where
    go [] r = r
    go ((a, b):xs) r = go xs (agregarClave a b r)

    agregarClave a b [] = [(a, [b])]
    agregarClave a b ((a', bs):r)
      | a == a'   = (a, b:bs) : r
      | otherwise = (a', bs) : agregarClave a b r

acumularClaves' :: Ord a => [(a, b)] -> Map a [b]
acumularClaves' xs = go xs empty
  where
    go [] r = r
    go ((a, b):xs) r = go xs (agregarClave a b r)

    agregarClave a b r = case Data.Map.lookup a r of
      Nothing -> insert a [b] r
      Just bs -> insert a (b:bs) r

memtriang :: Int -> Map Int Int
memtriang n = go 1 (insert 0 0 empty)
  where
    go k map
      | k == n + 1 = map
      | otherwise =
        let new = k + map ! (k - 1) in
        go (k + 1) (insert k new map)

cache :: Map Int Int
cache = memtriang 1000000

fasttriang :: Int -> Int
fasttriang = (cache !)

-- La primera vez que llamamos a la función, la referencia al valor de la caché no es computada
-- por lo que el tiempo de ejecución es mucho mayor.

-- Como la segunda vez, el map fue construido iterativamente, completo, el tiempo de ejecución es
-- el usual para los maps todas las veces.

-- lo construimos al revés porque es más rápido, la búsqueda es la que haremos lenta
memtriangSlow :: Int -> [(Int, Int)]
memtriangSlow n = reverse $ go 1 [(0, 0)]
  where
    go k map
      | k == n + 1 = map
      | otherwise =
        let new = k + snd (head map) in
        go (k + 1) ((k, new):map)

cacheSlow :: [(Int, Int)]
cacheSlow = memtriangSlow 1000000

fasttriangSlow :: Int -> Int
fasttriangSlow n = buscar cacheSlow
  where
    buscar [] = error "No se encontró el valor"
    buscar ((k, v):xs)
      | k == n = v
      | otherwise = buscar xs
