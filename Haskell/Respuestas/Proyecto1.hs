
-- 1) 
-- a)
esCero :: Int -> Bool
esCero x | x == 0 = True
         | x /= 0 = False

-- b)
esPositivo :: Int -> Bool
esPositivo x | x > 0 = True
             | x <= 0 = False

-- c)
esVocal :: Char -> Bool
esVocal x  = (x == 'a') || (x == 'e') || (x == 'i') || (x == 'o') || (x == 'u')

-- 2) 
-- a)
paratodo :: [Bool] -> Bool
paratodo [] = True
paratodo (x:xs) = x && paratodo xs

-- b)
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

-- c)
productoria :: [Int] -> Int
productoria [] = 1
productoria (x:xs) = x * productoria xs

-- d)
factorial :: Int -> Int
factorial 0 = 1
factorial x = x * (factorial (x-1))

-- e)
promedio :: [Int] -> Int
promedio (x:xs) = div (sumatoria (x:xs)) (length(x:xs))

-- 3)
pertenece :: Int -> [Int] -> Bool
pertenece n [] = False
pertenece n (x:xs) = (x == n) || pertenece n xs

-- 4) 
-- a)
paratodo' :: [a] -> (a -> Bool) -> Bool
paratodo' [] n = True
paratodo' (x:xs) n = (n x) && paratodo' xs n

-- b)
existe' :: [a] -> (a -> Bool) -> Bool
existe' [] n = False
existe' (x:xs) n = (n x) || existe' xs n 

-- c)
sumatoria' :: [a] -> (a -> Int) -> Int
sumatoria' [] n = 0
sumatoria' (x:xs) n = (n x) + sumatoria' xs n

-- d)
productoria' :: [a] -> (a -> Int) -> Int
productoria' [] n = 1
productoria' (x:xs) n = (n x) * productoria' xs n

-- 5)
paratodo''' :: [Bool] -> Bool
paratodo''' xs = paratodo' (xs) (==True)

-- 6)
-- a)
esPar :: Int -> Bool
esPar x = (mod x 2 == 0) && True

todosPares :: [Int] -> Bool
todosPares [] = True
todosPares (x:xs) = paratodo' (xs) (esPar)

-- b)
multiplo :: Int -> Int -> Bool
multiplo x n = (mod n x == 0)

hayMultiplo :: Int -> [Int] -> Bool
hayMultiplo n [] = False
hayMultiplo n (x:xs) =  existe' (x:xs) (multiplo n)

-- c)
dosCua :: Int -> Int
dosCua v = (v * v)

sumaCuadrados :: Int -> Int
sumaCuadrados n = sumatoria' ([1..n]) (dosCua)

-- d)
factorial' :: Int -> Int
factorial' n = productoria' ([1..n]) (+0)

-- e)
filtraPares:: [Int] -> [Int]
filtraPares [] = []
filtraPares (x:xs) | mod x 2 == 0 = x : filtraPares xs
                   | mod x 2 /= 0 = filtraPares xs

multiplicaPares :: [Int] -> Int
multiplicaPares [] = 0
multiplicaPares (x:xs) = productoria' (x : filtraPares xs) (*1) 

-- 7)
-- ¿Qué hace la función map?
-- Map es una función que te permite transformar los elementos de una lista y que devuelve una nueva lista con los elementos transformados.

-- ¿Qué hace la función filter?
-- La función filtra los elementos de una lista con condiciones específicas, y devuelve otra lista solo con los elementos de ella que cumplan esa condición.

-- ¿A qué equivale la expresión map succ [1, -4, 6, 2, -8], donde n = n + 1?
-- Equivale a: [2,-3,7,3,-7]
-- Lo que veo es que ésta expresión equivale a sumar uno a cada uno de los elementos de la lista.

-- ¿A qué equivale la expresión filter esPositivo [1, -4, 6, 2, -8]?
-- Equivale a: [1,6,2]
-- Esta expresión lo que hace es aplicar la función filter de esPositivo a cada elemento de una lista, o sea, filtra los elementos de una lista con la condición de la función esPositivo (o sea, si es positivo, entra en la nueva lista si no, no).

-- 8) 
-- a)
duplica :: [Int] -> [Int]
duplica [] = []
duplica (x:xs) = 2 * x : duplica xs

-- b)
duplica' :: [Int] -> [Int]
duplica' [] = []
duplica' (x:xs) = map (*2) (x:xs)

-- 9)
-- a)
pares :: [Int] -> [Int]
pares [] = []
pares (x:xs) | mod x 2 == 0 = x : pares xs
             | mod x 2 /= 0 = pares xs

--b)
pares':: [Int] -> [Int]
pares' [] = []
pares' (x:xs) = filter (esPar) (x:xs)

-- c)
-- La podría mejorar con la función filter y me evitaría hacer varios casos, la haría asi:
multiplicaPares' :: [Int] -> Int
multiplicaPares' [] = 0
multiplicaPares' (x:xs) = productoria' (filter (esPar) (x:xs)) (*1)

-- 10)
-- a)
primIgualesA :: Eq a => a -> [a] -> [a]
primIgualesA n [] = []
primIgualesA n (x:xs) | (x /= n) = [] 
primIgualesA n (x:xs) | (x == n) = (x : primIgualesA n xs)

-- b)
primIgualesA' :: Eq a => a -> [a] -> [a]
primIgualesA' n [] = []
primIgualesA' n (x:xs) = takeWhile (== n) (x : primIgualesA' n xs)

-- 11)
-- a)
primIguales :: Eq a => [a] -> [a]
primIguales [] = []
primIguales (x:[]) = [x]
primIguales (y:(x:xs)) | y == x = x : primIguales (y:xs)
                       | y /= x = y : primIguales [] 

-- b)
primIguales' :: Eq a => [a] -> [a]
primIguales' [] = []
primIguales' (x:xs) = primIgualesA (x) (x : xs)

-- 12)
cuantGen :: (b -> b -> b) -> b -> [a] -> (a -> b) -> b
cuantGen op z [] t = z
cuantGen op z (x:xs) t =  op (t x) (cuantGen op z xs t)

paratodo'' :: [a] -> (a -> Bool) -> Bool
paratodo'' xs t = cuantGen (&&) (True) (xs) (t)

existe'' :: [a] -> (a -> Bool) -> Bool
existe'' xs t = cuantGen (||) (False) (xs) (t)

sumatoria'' :: [a] -> (a -> Int) -> Int
sumatoria'' xs t = cuantGen (+) (0) (xs) (t)

productoria'' :: [a] -> (a -> Int) -> Int
productoria'' xs t = cuantGen (*) (1) (xs) (t)

-- 13)

-- a) Si está bien tipado, los tipos de cada subexpresión son: (x,y),x,y. El patrón cubre todos los casos de definición.

-- b) No está bien tipado.

-- c) No está bien tipado.

-- d) No está bien tipado.

-- e) Si está bien tipado, los tipos de cada subexpresión son:[(0,a)],(0,a),0,a. El patrón no cubre todos los casos de definición.
 
-- f) No está bien tipado.

-- g) No está bien tipado.

-- h) No está bien tipado.

-- i) No está bien tipado.

-- 14) 

-- a) f (x,y) = y . No podría dar otras definiciones alternativas, solo cambiando los nombres de las variables, pero la definición sería la misma.

-- b) No es posible dar una definición ya que c puede ser cualquier tipo de transformación entre a y b o incluso otra cosa totalmente diferente.

-- c)  f x y = x y . No podría dar otras definiciones alternativas.
 
-- d) No es posible dar una definición.
 
-- e) No es posible dar una definición. 