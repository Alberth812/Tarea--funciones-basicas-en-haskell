import Data.Ord (comparing)
import Data.Foldable (minimumBy, maximumBy)
import Data.ByteString (sort)
import Data.List (sortBy)

------------------------------------------------------------------
--Autor: Hernández Sosa Andres
--Materia: Programacion logica y funcional

-------------------------Funciones Básicas------------------------
--Ejercicio 1
promedio3 :: (Fractional a) => a -> a -> a -> a
promedio3 x y z = (x + y + z) / 3

--Ejercicio 2
sumaMonedas :: Int -> Int -> Int -> Int -> Int -> Int
sumaMonedas a b c d e = a * 1 + b * 2 + c * 5 + d * 10 + e * 20

-----------------------------------

--Ejercicio 3
volumenEsfera :: Float -> Float
volumenEsfera r = (4 / 3) * pi * r^3

-----------------------------------

--Ejercicio 4
areaDeCoronaCircular :: Float -> Float -> Float
areaDeCoronaCircular r1 r2 = pi * (r2^2 - r1^2)

--Ejercicio 5
ultimaCifra :: Integral a => a -> a
ultimaCifra x = x `rem` 10

--Ejercicio 6
maxTres :: Ord a => a -> a -> a -> a
maxTres x y z = max x (max y z)

--Ejercicio 7
rota1 :: [a] -> [a]
rota1 xs = tail xs ++ [head xs]

-----------------------------------

--Ejercicio 8
rota :: Int -> [a] -> [a]
rota n xs = drop n xs ++ take n xs

--Ejercicio 9
rango :: (Ord a) => [a] -> [a]
rango xs = [minimum xs, maximum xs]

--Ejercicio 10
palindromo :: (Eq a) => [a] -> Bool
palindromo xs = xs == reverse xs

--Ejercicio 11
interior :: [a] -> [a]
interior xs = tail (init xs)

--Ejercicio 12
linea :: Integer -> [Integer]
linea n = [Sum [1..(n-1)] + 1 .. sum [1..n]]

--Ejercicio 13
segmento :: Int -> Int -> [a] -> [a]
segmento m n xs = take (n - m + 1) (drop m xs)

--Ejercicio 14
extremos :: Int -> [a] -> [a]
extremos n xs = take n xs ++ reverse (take n (reverse xs))

-----------------------------------

--Ejercicio 15
mediano :: (Ord a, Num a) => a -> a -> a -> a
mediano x y z = x + y + z - maximum [x, y, z] - minimum [x, y, z]

--Ejercicio 16
tresIguales :: (Eq a) => a -> a -> a -> Bool
tresIguales x y z = (x == y) && (y == z)

--Ejercicio 17
tresDiferentes :: (Eq a) => a -> a -> a -> Bool
tresDiferentes x y z = (x /= y) && (y /= z) && (x /= z)

--Ejercicio 18
cuatroIguales :: (Eq a) => a -> a -> a -> a -> Bool
cuatroIguales x y z u = tresIguales x y z && (z == u)


------------------------- Guardas y Patrones ------------------------

--Ejercicio 1
divisionSegura :: Double -> Double -> Double
divisionSegura x y = if y == 0 then 9999 else x / y

--Ejercicio 2
xor1 :: Bool -> Bool -> Bool
xor1 x y = (x || y) && not (x && y)


-----------------------------------

--Ejercicio 3
mayorRectangulo :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
mayorRectangulo (a, b) (c, d) = if a * b >= c * d then (a, b) else (c, d)

-----------------------------------

--Ejercicio 4
intercambia :: (a, b) -> (b, a)
intercambia (x, y) = (y, x)

--Ejercicio 5
distancia :: (Double, Double) -> (Double, Double) -> Double
distancia (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

--Ejercicio 6
ciclo :: [a] -> [a]
ciclo [] = []
ciclo [x] = [x]
ciclo xs = last xs : init xs

-----------------------------------

--Ejercicio 7
numeroMayor :: (Num a, Ord a) => a -> a -> a 
numeroMayor x y
 | 10*x+y > 10*y+x = 10*x+y
 | otherwise = 10*y+x

-----------------------------------

--Ejercicio 8
numerodeRaices :: (Floating t, Ord t) => t -> t -> t -> Int
numerodeRaices a b c
 | d > 0 = 2
 | d == 0 = 1
 | otherwise = 0
 where
 d = b^2 - 4 * a * c

-----------------------------------

--Ejercicio 9
raices :: Double -> Double -> Double -> [Double]
raices a b c
  | discriminant > 0 = [(-b + sqrt discriminant) / (2*a), (-b - sqrt discriminant) / (2*a)]
  | discriminant == 0 = [(-b) / (2*a)]
  | otherwise = []
  where
    discriminant = b^2 - 4*a*c

-----------------------------------

--Ejercicio 10
area :: Double -> Double -> Double -> Double
area a b c = sqrt (s * (s - a) * (s - b) * (s - c))
  where s = (a + b + c) / 2

-----------------------------------

--Ejercicio 11
interseccion :: Ord a => [a] -> [a] -> [a]
interseccion [] _ = []
interseccion _ [] = []
interseccion [a,b] [c,d]
  | a > d || c > b = []
  | otherwise = [max a c, min b d]

-----------------------------------

--Ejercicio 12
linea :: Integer -> [Integer]
linea n = [sum [1..(n-1)] + 1 .. sum [1..n]]


---------------------------Recursividad -----------------------

--Ejercicio 1
potencia :: Integer -> Integer -> Integer
potencia _ 0 = 1
potencia x n = x * potencia x (n - 1)

--Ejercicio 2
mcd :: Integer -> Integer -> Integer
mcd a 0 = a
mcd a b = mcd b (a `mod` b)

-----------------------------------

--Ejercicio 3
pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece x (y:ys) = x == y || pertenece x ys

-----------------------------------

--Ejercicio 4
tomar :: Int -> [a] -> [a]
tomar _ [] = []
tomar 0 _ = []
tomar n (x:xs) = x : tomar (n - 1) xs

-----------------------------------

--Ejercicio 5
digitosC :: Integer -> [Integer]
digitosC n = [read [c] | c <- show n]

-----------------------------------

--Ejercicio 6
sumaDigitosR :: Integer -> Integer
sumaDigitosR 0 = 0
sumaDigitosR n = (n `mod` 10) + sumaDigitosR (n `div` 10)

-----------------------------------

--Ejercicio 2.1
ordenaRapida :: Ord a => [a] -> [a]
ordenaRapida [] = []
ordenaRapida (x:xs) =
  ordenaRapida [y | y <- xs, y <= x] ++ [x] ++ ordenaRapida [y | y <- xs, y > x]


---------------- Nuevos tipos de datos -------------------

data Estudiante = Estudiante {
  nombre :: String,
  apellido :: String,
  edad :: Int,
  numeroControl :: Int
} deriving (Show, Eq, Ord)

-- Lista de estudiantes modificada
estudiantes :: [Estudiante]
estudiantes = [
  Estudiante "Karla" "Mendoza" 21 2003,
  Estudiante "Diego" "Fernandez" 20 2004,
  Estudiante "Alejandro" "Ruiz" 23 2001,
  Estudiante "Natalia" "Gomez" 22 2002,
  Estudiante "Samuel" "Santos" 19 2005,
  Estudiante "Paola" "Cruz" 18 2006,
  Estudiante "Daniel" "Castillo" 21 2003,
  Estudiante "Valeria" "Martinez" 20 2004,
  Estudiante "Oscar" "Rivas" 19 2005,
  Estudiante "Lorena" "Serrano" 22 2002
  ]

-- Ordenar lista de estudiantes por edad
listaOrdenada :: [Estudiante]
listaOrdenada = sortBy (comparing edad) estudiantes

-- Estudiante más joven
estudianteMenor :: Estudiante
estudianteMenor = minimumBy(comparing edad) estudiantes

-- Estudiante más mayor
estudianteMayor :: Estudiante
estudianteMayor = maximumBy (comparing edad) estudiantes

-- Promedio de edades
promedioEdades :: Double
promedioEdades = fromIntegral (sum (map edad estudiantes)) / fromIntegral (length estudiantes)


---------------- Arboles -------------------

data Arbol a = Hoja | Nodo a (Arbol a) (Arbol a) deriving (Show, Eq)

-- Función para insertar un elemento en el árbol
insertar :: (Ord a) => a -> Arbol a -> Arbol a
insertar x Hoja = Nodo x Hoja Hoja
insertar x (Nodo a izq der)
  | x < a     = Nodo a (insertar x izq) der
  | x > a     = Nodo a izq (insertar x der)
  | otherwise = Nodo a izq der

-- Función para insertar elementos desde un arreglo
insertarDesdeArreglo :: (Ord a) => [a] -> Arbol a
insertarDesdeArreglo = foldr insertar Hoja

-- Función para buscar un elemento en el árbol
buscar :: (Ord a) => a -> Arbol a -> Bool
buscar _ Hoja = False
buscar x (Nodo a izq der)
  | x == a    = True
  | x < a     = buscar x izq
  | otherwise = buscar x der

-- Recorridos del árbol
inorden :: Arbol a -> [a]
inorden Hoja = []
inorden (Nodo a izq der) = inorden izq ++ [a] ++ inorden der

preorden :: Arbol a -> [a]
preorden Hoja = []
preorden (Nodo a izq der) = [a] ++ preorden izq ++ preorden der

posorden :: Arbol a -> [a]
posorden Hoja = []
posorden (Nodo a izq der) = posorden izq ++ posorden der ++ [a]
