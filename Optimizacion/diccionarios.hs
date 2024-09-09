data Contacto = CDatos String String String String ...
type Tel = Int
type Agenda = [(Tel, Contacto)]

buscarContacto :: Tel -> Agenda -> Maybe Contacto
buscarContacto telefono [] = Nothing
buscarContacto telefono ((t, c):agenda)
  | t == telefono = Just c
  | otherwise = buscarContacto telefono agenda

agregarContacto :: Tel -> Contacto -> Agenda -> Agenda
agregarContacto t c agenda = (t, c):: agenda

borrarContacto :: Tel -> Agenda -> Agenda
borrarContacto telefono [] = []
borrarContacto telefono ((t, c): agenda)
  |telefono == t = agenda
  |otherwise = (t, c) : (borrarContacto telefono agenda)