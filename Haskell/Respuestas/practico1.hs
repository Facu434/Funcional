entre0y9 :: Int -> Bool
entre0y9 x | x > 0 && x < 9 = True
           | x >= 9 = False
           | x <= 0 = False

rangoPrecio :: Int -> String
rangoPrecio x | x >= 0 && x < 2000 = "Muy barato"
              | x >= 2000 && x < 5000 = "Hay que verlo bien"
              | x > 5000 = "Demasiado caro"
              | x < 0 = "Esto no puede ser"

absoluto :: Int -> Int 
absoluto x | x >= 0 = x
           | x < 0 = (-1 * x)

esMultiplo2 :: Int -> Bool
esMultiplo2 x | mod x 2 == 0 = True
              | mod x 2 /=0 = False

esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y | mod y x == 0 = True
                 | mod y x /= 0 = False 

esBisiesto :: Int -> Bool 
esBisiesto x = mod x 100 /= 0 && (mod x 4 == 0 || mod x 400 == 0)

dispersion :: Int -> Int -> Int -> Int 
dispersion x y z = (max z (max x y) - min z (min x y))

celciusToFahr :: Double -> Double
celciusToFahr x = (x * 1.8) + 32

fahrToCelcius :: Double -> Double 
fahrToCelcius x = (x - 32) * 1.8

haceFrioF :: Double -> Bool
haceFrioF x | x < 46.4 = True
            | x >= 46.4 = False

segundo3 :: (Int, Int, Int) -> Int
segundo3 (a, b , c) = b

ordena :: (Int, Int) -> (Int, Int)
ordena (a, b) = (min a b , max a b)

rangoPrecioParametrizado :: Int -> (Int, Int) -> String
rangoPrecioParametrizado x (a, b) | (x < a && x > 0) = "Muy barato"
                                  | (x >= a && x <= b) = "Hay que verlo bien"
                                  | (x > b) = "Demasiado caro"
                                  | (x <= 0) = "Esto no puede ser"


mayor3 :: (Int, Int, Int) -> (Bool, Bool, Bool)
mayor3 (a, b, c) = (a > 3, b > 3, c > 3)

todosIguales :: (Int, Int, Int) -> Bool
todosIguales (a, b, c) = ((a == b) && (a ==c))