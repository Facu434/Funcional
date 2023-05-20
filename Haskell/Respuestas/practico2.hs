soloPares :: [Int] -> [Int]
soloPares [] = []
soloPares (x:xs) | (mod x 2 == 0) = x : soloPares xs
                 | (mod x 2 /= 0) = soloPares xs

mayoresQue10 :: [Int] -> [Int]
mayoresQue10 [] = []
mayoresQue10 (x:xs) | (x > 10) = x : mayoresQue10 xs
                    | (x <= 10) = mayoresQue10 xs

mayoresQue :: Int -> [Int] -> [Int]
mayoresQue n [] = []
mayoresQue n (x:xs) | (x > n) = x : mayoresQue n xs
                    | (x <= n) = mayoresQue n xs


sumar1 :: [Int] -> [Int]
sumar1 [] = []
sumar1 (x:xs) = (x + 1) : sumar1 xs

duplica :: [Int] -> [Int]
duplica [] = []
duplica (x:xs) = (x * 2) : duplica xs

multiplica :: Int -> [Int] -> [Int]
multiplica n [] = []
multiplica n (x:xs) = (x * n) : multiplica n xs


todosMenores10 :: [Int] -> Bool
todosMenores10 [] = True
todosMenores10 (x:xs) = (x < 10) && todosMenores10 xs

hay0 :: [Int] -> Bool
hay0 [] = False
hay0 (x:xs) = (x == 0) || hay0 xs

suma :: [Int] -> Int
suma [] = 0
suma (x:xs) = x + suma xs


repartir :: [String] -> [String] -> [(String, String)]
repartir [] [] = []
repartir (x:xs) [] = []
repartir [] (y:ys) = []
repartir (x:xs) (y:ys) = (x , y) : repartir xs ys

apellidos :: [(String, String, Int)] -> [String]
apellidos [] = []
apellidos ((x,y,z):xs) = y : apellidos xs

cardinal :: [a] -> Int
cardinal [] = 0
cardinal (x:xs) =  1 + cardinal xs

indice :: [a] -> Int -> a
indice (x:xs) 0 = x 
indice (x:xs) n = indice xs (n - 1)

pIzq ::  [a] -> a -> [a]
pIzq [] a = [a]
pIzq (x:xs) a =  x : pIzq xs a

tomar :: [a] -> Int -> [a]
tomar [] n = []
tomar (x:xs) n = x : tomar xs (n - 1)

soltar :: [a] -> Int -> [a]
soltar [] 0 = []
soltar (x:xs) 0 = x : soltar xs 0
soltar (x:xs) n = soltar xs (n - 1)

concatenar :: [a] -> [a] -> [a] 
concatenar [] [] = []
concatenar (x:xs) [] = (x:xs) 
concatenar [] (y:ys) = (y:ys)
concatenar (x:xs) (y:ys) = x : concatenar xs (y:ys)


maximo :: [Int] -> Int 
maximo [x] = x 
maximo (x:xs) = max x (maximo xs)

sumaPares :: [(Int, Int)] -> Int 
sumaPares [] = 0
sumaPares ((x,y):xs) = x + y + sumaPares xs

todos0y1 :: [Int] -> Bool
todos0y1 [] = True
todos0y1 (x:xs) = (x == 0 || x == 1) && todos0y1 xs

quitar0s :: [Int] -> [Int]
quitar0s [] = []
quitar0s (x:xs) | (x /= 0) = x : quitar0s xs
                | (x == 0) = quitar0s xs

ultimo :: [a] -> a
ultimo [x] = x 
ultimo (x:xs) = ultimo xs

repetir :: Int -> Int -> [Int]
repetir k 0 = []
repetir k n = k : repetir (n - 1) n

concatena :: [[a]] -> [a]
concatena [] = []
concatena (x:xs) = x ++ concatena xs

rev :: [Int] -> [Int]
rev [] = []
rev (x:xs) = rev xs ++ [x]