data Contacto = CDatos String String String String
type Tel = Int
type Agenda = [(Tel, Contacto)]

buscarContacto :: Tel -> Agenda -> Maybe Contacto
buscarContacto telefono [] = Nothing
buscarContacto telefono ((t, c):agenda)
  | t == telefono = Just c
  | otherwise = buscarContacto telefono agenda

agregarContacto :: Tel -> Contacto -> Agenda -> Agenda
agregarContacto t c agenda = (t, c): agenda

borrarContacto :: Tel -> Agenda -> Agenda
borrarContacto telefono [] = []
borrarContacto telefono ((t, c): agenda)
  |telefono == t = agenda
  |otherwise = (t, c) : (borrarContacto telefono agenda)

acumulacionClaves:: [(a,b)] -> [(a, [b])]
acumulacionClaves [] = []
acumulacionClaves ((k, v):xs) = go k v xs []
  where
    go k v [] resultado = (k ,[v]) : resultado
    go k v ((k', v'):xs) resultado
      |k == k' = go k v xs ((k', v':v) : resultado)
      |otherwise = go k v xs ((k', [k']) : resultado)
