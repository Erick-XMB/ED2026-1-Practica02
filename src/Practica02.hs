module Practica02 where

--BINARIOS
data Bit = O | I 
        deriving (Show, Eq)

type Binario = [Bit]

--BINARIOS

--- Función auxiliar ----
--- Se usó para invertir el orden de una lista ---
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

--- Función auxiliar ----
--- Se usó para convertir un BIT (I/O) en un entero ---
bitToDecimal :: Bit -> Int
bitToDecimal O = 0
bitToDecimal I = 1

--- Función auxiliar ----
--- Se usó para convertir Binario a decimal con el bit menos significativo a la izquierda ---
btd :: Binario -> Int
btd [] = 0  
btd [O] = 0
btd [I] = 1
btd (x:xs) = (2 * (btd xs)) + btd [x] 

-- Ejercicio No 1 "to decimal" (Dado un número binario, esta función calcula el valor decimal de ese número.)
toDecimal :: Binario -> Int
toDecimal xs = btd (myReverse xs) 


--- Función auxiliar  ----
--- Se usó para convertir un decimal a BIT (O/I) ---
decimalToBit :: Int -> Bit
decimalToBit 0 = O
decimalToBit 1 = I

-- Ejercicio No 2 "to bin" (Dados número en representación decimal, esta función devuelve la representación binaria de ese número)
toBin :: Int -> Binario
toBin 0 = [O] 
toBin 1 = [I] 
toBin n = (toBin (n `div` 2)) ++ [decimalToBit (n `mod` 2)]


--- Función auxiliar ----
--- Se usó para aumentar en una unidad un Binario ---
increment :: Binario -> Binario
increment []     = [I]              -- si no hay nada, 1
increment (O:xs) = I : xs           -- 0 + 1 = 1, no hay carry
increment (I:xs) = O : increment xs -- 1 + 1 = 0 y carry a lo que sigue

--- Función auxiliar ----
--- Se usó para sumar considerando el bit menos significativo a la izquierda ---
add :: Binario -> Binario -> Binario
add [] ys = ys
add xs [] = xs
add (O:xs) (O:ys) = O:add xs ys 
add (I:xs) (O:ys) = I:add xs ys 
add (O:xs) (I:ys) = I:add xs ys 
add (I:xs) (I:ys) = O:increment (add xs ys)

-- Ejercicio No 3 "suma" (La función recibe dos números binarios y regresa la suma de los mismos.)
suma :: Binario -> Binario -> Binario
suma x y = reverse (add (reverse x) (reverse y)) 


--LISTAS

--- Función auxiliar ----
--- Se usó para obtener el ultimo elemento de una lista ---
myLast :: [a] -> a
myLast [x] = x 
myLast (x:xs) = myLast xs

--- Función auxiliar ----
--- Se usó para eliminar el ultimo elemento de una lista ---
removeLast :: [a] -> [a]
removeLast [] = []
removeLast [a] = []
removeLast (x:xs) = x: removeLast xs

-- Ejercicio No 1 "Palindromo" (Dada una lista, esta función devuelve True si la lista es un palíndromo, False en otro caso.)
palindromo :: Eq a => [a] -> Bool
palindromo [] = True
palindromo [x] = True
palindromo [x, y] = if x == y then True else False
palindromo (x:xs) = if (x == (myLast xs)) then
                        palindromo (removeLast xs)
                    else
                        False


--- Función auxiliar ----
--- Se usó para determinar si un elemento pertenece o no a una lista ---
isIn :: Eq a => a -> [a] -> Bool
isIn x [] = False 
isIn x [y] = if x == y then True else False
isIn x (y:ys) = if x == y then True else isIn x ys

--- Función auxiliar ----
--- Se usó para eliminar un elemento particular de una lista ---
borrarElemento :: Eq a => a -> [a] -> [a]
borrarElemento _ [] = []
borrarElemento x [y] = if x == y then [] else [y]
borrarElemento x (y:ys) = if (isIn x (y:ys)) then borrarElemento x ys else (y:ys)

--- Función auxiliar ----
--- Se usó para calcular la diferencia de 2 listas ---
diferencia :: Eq a => [a] -> [a] -> [a]
diferencia [] [] = []
diferencia x [] = x
diferencia [] y = []
diferencia [x] [y] = if x == y then [] else [x]
diferencia (x:xs) (y:ys) = if isIn x (y:ys) then 
                                                diferencia (xs) (y:ys)
                                            else
                                                (x:diferencia xs (y:ys))
                            

--Ejercicio No 2 "Diferencia Simetrica"(Funcion principal que calcula la diferencia simetrica (Aquellos elementos que esten en la union pero no en la interseccion))
diferenciaSimetrica :: Eq a => [a] -> [a] -> [a]
diferenciaSimetrica x y = (diferencia x y) ++ (diferencia y x)


--Ejercicio No 3 "Conjunto potencia" (Dada una lista, esta función calcula el conjunto potencia de la lista ingresada.)
conjuntoPotencia :: [a] -> [[a]]
conjuntoPotencia [] = [[]] 
conjuntoPotencia [x] = [[], [x]] 
conjuntoPotencia (x:xs) = conjuntoPotencia xs ++ [ x : p | p <- conjuntoPotencia xs ]  


--LISTAS DE LONGITUD PAR
--Esta va de regalo
type ListaPar a b = [(a,b)]

--Ejercicio No 1 "Longitud" (La función recibe una lista de pares y devuelve la longitud.)
longitud :: ListaPar a b -> Int
longitud [] = 0
longitud [x] = 2  
longitud (x:xs) = 2 + longitud xs  

--Ejercicio No 2 "Map" (Usa la idea de la funcion Map ............)
myMap :: (a -> c) -> (b -> d) -> ListaPar a b -> ListaPar c d 
myMap _ _ [] = [] 
myMap f g ((x,y):xs) = (f x, g y) : myMap f g xs

--Sumar pares 
sumaPares :: ListaPar a b -> (a,b)
sumaPares [] = (0,0)
sumaPares [x:xs] [y:ys] = (x+y, sumaPares(xs +ys))

--Ejercicio No 4 "Filter pares" (Basandose en la funcion filter, filtra los numeros x,y pares de una lista, donde los dos son pares, y los devuelve en otra lista)
myFilter :: ((a,b) -> Bool) -> ListaPar a b -> ListaPar a b
myFilter p [] = []
myFilter p ((x,y):xs) = if (p (x,y)) then (x,y) : myFilter p xs else (myFilter p xs)