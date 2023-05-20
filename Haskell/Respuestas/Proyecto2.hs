----- 1)
-- a)
data Carrera = Matematica | Fisica | Computacion | Astronomia deriving (Show, Eq, Ord)

--b)
titulo :: Carrera -> String
titulo Matematica = "Licenciatura en Matematica"
titulo Fisica = "Licenciatura en Fisica"
titulo Computacion = "Licenciatura en Ciencias de La Computacion"
titulo Astronomia = "Licenciatura en Astronomia"

-- c)
data NotaBasica = Do | Re | Mi | Fa | Sol | La | Si deriving (Show, Eq, Ord)

-- d)
cifradoAmericano :: NotaBasica -> Char
cifradoAmericano Do = 'C'
cifradoAmericano Re = 'D'
cifradoAmericano Mi = 'E'
cifradoAmericano Fa = 'F'
cifradoAmericano Sol = 'G'
cifradoAmericano La = 'A'
cifradoAmericano Si = 'B'

----- 3)
-- a)
minimoElemento :: Ord a => Eq a => [a] -> a 
minimoElemento (x:[]) = x 
minimoElemento ((y:x:xs)) = min y (minimoElemento (x:xs))


-- b)
minimoElemento' :: (Bounded a, Ord a)  => [a] -> a
minimoElemento' [] = maxBound 
minimoElemento' (x:xs) = min x ( minimoElemento' xs)

-- c)
-- La nota mas grave de la melodía [Fa, La, Sol, Re, Fa] es Re

----- 4)
-- a)
type Ingreso = Int

data Cargo = Titular | Asociado | Adjunto | Asistente | Auxiliar deriving (Eq, Show, Ord)

data Area = Administrativa | Ensenanza | Economica | Postgrado deriving (Eq, Show, Ord)

data Persona = Decane | Docente Cargo | NoDocente Area | Estudiante Carrera Ingreso deriving (Eq, Show, Ord)

-- b) 
-- Docente es de tipo Cargo -> Persona

-- c)
cuantos_doc :: [Persona] -> Cargo -> Int
cuantos_doc [] c = 0 
cuantos_doc (Docente f : xs) c | f ==  c = 1 + cuantos_doc xs c 
                               | f /=  c = 0 + cuantos_doc xs c 
cuantos_doc (x : xs) c = 0 + cuantos_doc xs c 

-- d)
cuantos_doc' :: [Persona] -> Cargo -> Int
cuantos_doc' [] c = 0
cuantos_doc' (x:xs) c = length  (filter (== Docente c ) (x:xs))

----- 5)
-- a)
data Alteracion = Bemol | Sostenido | Natural 
data NotaMusical = Nota NotaBasica Alteracion    

sonido :: NotaBasica -> Int
sonido Do = 1 
sonido Re = 3 
sonido Mi = 5 
sonido Fa = 6
sonido Sol = 8 
sonido La = 10 
sonido Si = 12 

-- b)
sonidoCromatico :: NotaMusical -> Int
sonidoCromatico (Nota x Sostenido) = sonido x + 1
sonidoCromatico (Nota x Bemol) = sonido x - 1
sonidoCromatico (Nota x Natural) = sonido x 

-- c)
instance Eq NotaMusical
  where 
      n1 == n2 = sonidoCromatico n1 == sonidoCromatico n2

-- d)
instance Ord NotaMusical
  where
      c1 <= c2 = sonidoCromatico c1 <= sonidoCromatico c2

----- 6)
-- a)
primerElemento :: [a] -> Maybe a 
primerElemento [] = Nothing 
primerElemento (x:xs) = Just x 

----- 7)
data Cola = VaciaC | Encolada Persona Cola deriving (Eq, Show, Ord)

-- a)
-- 1)
atender :: Cola -> Maybe Cola 
atender VaciaC = Nothing
atender (Encolada p c) = Just c

-- 2)
encolar :: Persona -> Cola -> Cola
encolar x VaciaC = Encolada x VaciaC
encolar x (Encolada p c) = Encolada p (encolar x c)

-- 3)
atu :: Cola -> Cola
atu VaciaC = VaciaC
atu (Encolada p c) = c 

busca :: Cola -> Cargo -> Maybe Persona
busca VaciaC k = Nothing
busca (Encolada (Docente f) c) k | f == k = Just (Docente f) 
                                 | f /= k = busca (atu (Encolada (Docente f) c)) k 
busca (Encolada p c) k = busca (atu (Encolada p c)) k

-- b)

-- Cola se parece al tipo lista de a, osea: [a]

-- 8)
-- a)
data ListaAsoc a b = Vacia | Nodo a b (ListaAsoc a b) deriving (Show, Eq, Ord)

type NumeroT = ListaAsoc String Int

-- b)
-- 1)
la_long :: ListaAsoc a b -> Int
la_long  Vacia = 0 
la_long (Nodo x y Vacia) = 1 + la_long Vacia
la_long (Nodo x y (Nodo e f g)) = 1 + la_long (Nodo e f g) 

-- 2)
la_concat :: ListaAsoc a b -> ListaAsoc a b -> ListaAsoc a b
la_concat Vacia l2 = l2 
la_concat (Nodo x y z) l2 = Nodo x y (la_concat z l2) 

-- 3)
la_agregar :: ListaAsoc a b -> a -> b -> ListaAsoc a b 
la_agregar Vacia t r = (Nodo t r Vacia)
la_agregar (Nodo x y z) t r = Nodo x y (la_agregar z t r)

-- 4)
la_pares :: ListaAsoc a b -> [(a, b)]
la_pares Vacia = []
la_pares (Nodo x y z) = (x,y) : la_pares z

-- 5)
la_busca :: Eq a => ListaAsoc a b -> a -> Maybe b
la_busca Vacia c = Nothing
la_busca (Nodo x y z) c | c == x = Just y 
                        | c /= x = la_busca z c

-- 6)
la_borrar :: Eq a => a -> ListaAsoc a b -> ListaAsoc a b 
la_borrar c Vacia = Vacia
la_borrar c (Nodo x y z) | c /= x = (Nodo x y (la_borrar c z))
                         | c == x = la_borrar c z 

----- 9)
data Arbol a = Hoja | Rama ( Arbol a ) a ( Arbol a ) deriving (Show, Eq, Ord)

-- a)
a_long :: Arbol a -> Int
a_long Hoja = 0
a_long (Rama (Hoja) u (Hoja)) = 1 + a_long Hoja
a_long (Rama (Hoja) u (Rama y o o2))= 1 + a_long (Rama y o o2)
a_long (Rama (Rama x i i2) u (Rama y o o2))= 1 + a_long (Rama y o o2)

-- b)
a_hojas :: Arbol a -> Int
a_hojas Hoja = 1
a_hojas (Rama (Hoja) u (Hoja)) = 2 + a_hojas Hoja
a_hojas (Rama (Hoja) u (Rama y o o2)) = 1 + a_hojas (Rama y o o2)
a_hojas (Rama (Rama x i i2) u (Rama y o o2))= 1 + a_hojas (Rama y o o2)

-- c)
a_inc :: Num a => Arbol a -> Arbol a
a_inc (Rama (Hoja) u (Hoja)) = (Rama (Hoja) (u + 1) (Hoja))
a_inc (Rama (Hoja) u (Rama y o o2)) = (Rama (Hoja) (u + 1) (a_inc (Rama y o o2)))
a_inc (Rama (Rama x i i2) u (Rama y o o2)) = (Rama (Rama x (i + 1) i2) (u + 1) (a_inc (Rama y o o2)))

-- d)
a_map :: (a -> b) -> Arbol a -> Arbol b
a_map f (Rama (Hoja) u (Hoja)) = (Rama (Hoja) (f u) (Hoja))
a_map f (Rama (Hoja) u (Rama y o o2)) = (Rama (Hoja) (f u) (a_map f (Rama y o o2)))
a_map f (Rama (Rama x i i2) u (Rama y o o2)) = (Rama (a_map f (Rama x i i2)) (f u) (a_map f (Rama y o o2)))

a_inc' :: Num a => Arbol a -> Arbol a
a_inc' x = a_map (+1) (x)