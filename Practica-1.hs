--1. Defina las siguientes funciones:
sucesor :: Int -> Int
--Dado un número devuelve su sucesor
sucesor x = x + 1

sumar :: Int -> Int -> Int
--Dados dos números devuelve su suma utilizando la operación +.
sumar x y = x + y

maximo :: Int -> Int -> Int
--Dados dos números devuelve el mayor de estos.
maximo x y = if (x>y) then x else y

---------------------------------------------------------------------------------

--2. Defina las siguientes funciones utilizando pattern matching:
negar :: Bool -> Bool
--Dado un booleano, si es T rue devuelve F alse, y si es F alse devuelve T rue.
--Definida en Haskell como not.
negar True  = False
negar False = True

andLogico :: Bool -> Bool -> Bool
--Dados dos booleanos si ambos son T rue devuelve T rue, sino devuelve F alse.
--Definida en Haskell como &&.
andLogico True True = True
andLogico _	   _	= False

orLogico :: Bool -> Bool -> Bool
--Dados dos booleanos si alguno de ellos es T rue devuelve T rue, sino devuelve F alse.
--Definida en Haskell como ||.
orLogico False False = False
orLogico _     _     = True

primera :: (Int,Int) -> Int
--Dado un par de números devuelve la primera componente.
--Definida en Haskell como fst.
primera (x,y) = x

segunda :: (Int,Int) -> Int
--Dado un par de números devuelve la segunda componente.
--Definida en Haskell como snd.
segunda (x,y) = y

sumaPar :: (Int,Int) -> Int
--Dado un par de números devuelve su suma.
sumaPar (x,y) = x + y

maxDelPar :: (Int,Int) -> Int
--Dado un par de números devuelve el mayor de estos.
maxDelPar (x,y) = maximo x y

---------------------------------------------------------------------------------

--3. Defina las siguientes funciones polimórficas:
loMismo :: a -> a
--Dado un elemento de algún tipo devuelve ese mismo elemento.
loMismo x = x

siempreSiete :: a -> Int
--Dado un elemento de algún tipo devuelve el número 7.
siempreSiete x = 7

duplicar :: a -> (a,a)
--Dado un elemento de algún tipo devuelve un par con ese elemento en ambas compo-
--nentes.
duplicar x = (x,x)

singleton :: a -> [a]
--Dado un elemento de algún tipo devuelve una lista con este único elemento.
singleton x = x : []

---------------------------------------------------------------------------------

--4. Defina las siguientes funciones polimórficas utilizando pattern matching sobre listas:
isEmpty :: [a] -> Bool
--Dada una lista de elementos, si es vacía devuelve T rue, sino devuelve F alse.
isEmpty [] = True
isEmpty xs = False

heads :: [a] -> a
--Dada una lista devuelve su primer elemento.
--Precondición: la lista no puede ser vacía
heads (x:xs) = x

tails :: [a] -> [a]
--Dada una lista devuelve esa lista menos el primer elemento.
--Precondición: la lista no puede ser vacía
tails (x:xs) = xs

---------------------------------------------------------------------------------

--2. Recursión
--2.1. Recursión sobre listas
--Defina las siguientes funciones utilizando recursión estructural sobre listas, salvo que se indique
--lo contrario:
sumatoria :: [Int] -> Int
--Dada una lista de enteros devuelve la suma de todos sus elementos.
sumatoria []     = 0
sumatoria (x:xs) = x + sumatoria xs

longitud :: [a] -> Int
--Dada una lista de elementos de algún tipo devuelve el largo de esa lista, es decir, la cantidad
--de elementos que posee.
longitud []     = 0
longitud (x:xs) = 1 + longitud xs

mapSucesor :: [Int] -> [Int]
--Dada una lista de enteros, devuelve la lista de los sucesores de cada entero.
mapSucesor []     = []
mapSucesor (x:xs) = (x+1) : mapSucesor xs

mapSumaPar :: [(Int,Int)] -> [Int]
--Dada una lista de pares de enteros, devuelve una nueva lista en la que cada elemento es la
--suma de los elementos de cada par.
mapSumaPar []     = []
mapSumaPar (x:xs) = sumaPar x : mapSumaPar xs

mapMaxDelPar :: [(Int,Int)] -> [Int]
--Dada una lista de pares, devuelve una nueva lista en la que cada elemento es el mayor de
--las componentes de cada par.
mapMaxDelPar []     = []
mapMaxDelPar (x:xs) = maxDelPar x : mapMaxDelPar xs

todoVerdad :: [Bool] -> Bool
--Dada una lista de booleanos devuelve True si todos sus elementos son True.
todoVerdad []     = True
todoVerdad (x:xs) = x && todoVerdad xs

algunaVerdad :: [Bool] -> Bool
--Dada una lista de booleanos devuelve True si alguno de sus elementos es True.
algunaVerdad []     = False
algunaVerdad (x:xs) = x || algunaVerdad xs

pertenece :: Eq a => a -> [a] -> Bool
--Dados un elemento e y una lista xs devuelve True si existe un elemento en xs que sea igual a e.
pertenece e []     = False
pertenece e (x:xs) = if (e==x) then True else pertenece e xs

apariciones :: Eq a => a -> [a] -> Int
--Dados un elemento e y una lista xs cuenta la cantidad de apariciones de e en xs.
apariciones e []     = 0
apariciones e (x:xs) = if (e==x) then 1 + apariciones e xs else apariciones e xs

filtrarMenoresA :: Int -> [Int] -> [Int]
--Dados un número n y una lista xs, devuelve todos los elementos de xs que son menores a n.
filtrarMenoresA e []     = []
filtrarMenoresA e (x:xs) = if (e>x) then x : filtrarMenoresA e xs else filtrarMenoresA e xs
 
filtrarElemento :: Eq a => a -> [a] -> [a]
--Dados un elemento y una lista filtra (elimina) todas las ocurrencias de ese elemento en la lista.
filtrarElemento e []     = []
filtrarElemento e (x:xs) = if (e==x) then filtrarElemento e xs else x : filtrarElemento e xs

mapLongitudes :: [[a]] -> [Int]
--Dada una lista de listas, devuelve la lista de sus longitudes. Aplique esta función a la lista
--de strings ["Estructuras", "de", "datos"] y observe el resultado.
mapLongitudes []     = []
mapLongitudes (x:xs) = longitud x : mapLongitudes xs 

longitudMayorA :: Int -> [[a]] -> [[a]]
--Dados un número n y una lista de listas, devuelve la lista de aquellas listas que tienen más
--de n elementos.
longitudMayorA e []     = []
longitudMayorA e (x:xs) = if (longitud x > e) then x : longitudMayorA e xs else longitudMayorA e xs 

intercalar :: a -> [a] -> [a]
--Dado un elemento e y una lista xs, ubica a e entre medio de todos los elementos de xs.
--Ejemplo: intercalar ’,’ "abcde" == "a,b,c,d,e".
intercalar e []     = []
intercalar e [x]    = x : intercalar e []
intercalar e (x:xs) = x : [e] ++ intercalar e xs 

snoc :: [a] -> a -> [a]
--Dados una lista y un elemento, devuelve una lista con ese elemento agregado al final de la lista.
snoc [] e     = e : []
snoc (x:xs) e = x : snoc xs e

append :: [a] -> [a] -> [a]
--Dadas dos listas devuelve la lista con todos los elementos de la primera lista y todos los
--elementos de la segunda a continuación. Definida en Haskell como ++.
append xs []     = xs 
append [] ys     = ys
append (x:xs) ys = x : append xs ys 

aplanar :: [[a]] -> [a]
--Dada una lista de listas, devuelve una única lista con todos sus elementos.
aplanar []     = []
aplanar (x:xs) = heads x : append (tails x) (aplanar xs)

reversa :: [a] -> [a]
--Dada una lista devuelve la lista con los mismos elementos de atrás para adelante. Definida
--en Haskell como reverse.
reversa []     = []
reversa (x:xs) = append (reversa xs) (singleton x) 

zipMaximos :: [Int] -> [Int] -> [Int]
--Dadas dos listas de enteros, devuelve una lista donde el elemento en la posición
--n es el máximo entre el elemento n de la primera lista y de la segunda lista, teniendo en cuenta que
--las listas no necesariamente tienen la misma longitud.
zipMaximos [] []         = []
zipMaximos xs []         = xs
zipMaximos [] ys         = ys
zipMaximos (x:xs) (y:ys) = if (x>y) then y : append (singleton x) (zipMaximos xs ys) else x : append (singleton y) (zipMaximos xs ys)

zipSort :: [Int] -> [Int] -> [(Int, Int)]
--Dadas dos listas de enteros de igual longitud, devuelve una lista de pares 
--(min, max), donde min y max son el mínimo y el máximo entre los elementos de ambas listas en la misma posición.
zipSort [] []         = []
zipSort (x:xs) []     = (x,0) : zipSort xs []
zipSort [] (y:ys)     = (0,y) : zipSort [] ys
zipSort (x:xs) (y:ys) = if (x>y) then (y,x) : zipSort xs ys else (x,y) : zipSort xs ys

promedio :: [Int] -> Int
--Dada una lista de enteros, devuelve un número que es el promedio entre todos los elementos
--de la lista. ¿Pudo resolverla utilizando recursión estructural? No
promedio xs = div (sumatoria xs) (longitud xs) 

minimums :: Ord a => [a] -> a
--Dada una lista devuelve el mínimo
--Parcial en lista vacía
minimums [x]    = x
minimums (x:xs) = if (x < minimums xs) then x else minimums xs

--2.2.  Recursión sobre números
--Defina las siguientes funciones utilizando recursión sobre números enteros, salvo que se indique lo contrario:

factorial :: Int -> Int
--Dado un número n se devuelve la multiplicación de este número y todos sus anteriores hasta
--llegar a 0. Si n es 0 devuelve 1. La función es parcial si n es negativo.
factorial 0 = 1
factorial n = n * factorial (n-1)
 
cuentaRegresiva :: Int -> [Int]
--Dado un número n devuelve una lista cuyos elementos sean los números comprendidos entre
--n y 1 (incluidos). Si el número es inferior a 1, devuelve la lista vacía.
cuentaRegresiva 0 = []
cuentaRegresiva n = n : cuentaRegresiva (n-1)

contarHasta :: Int -> [Int]
--Dado un número n devuelve una lista cuyos elementos sean los números entre 1 y n (inclui-dos).
contarHasta n = reversa (cuentaRegresiva n)

replicarN :: Int -> a -> [a]
--Dado un número n y un elemento e devuelve una lista en la que el elemento e repite n veces.
replicarN 0 e = []
replicarN x e = e : replicarN (x-1) e

desdeHasta :: Int -> Int -> [Int]
--Dados dos números n y m devuelve una lista cuyos elementos sean los números entre n y m (incluidos).
desdeHasta n m = if (n==m) then singleton m else n : desdeHasta (n+1) m

takeN :: Int -> [a] -> [a]
--Dados un número n y una lista xs, devuelve una lista con los primeros n elementos de xs.
--Si xs posee menos de n elementos, se devuelve la lista completa.
takeN n []     = []
takeN 0 xs     = []
takeN n (x:xs) = x : takeN (n-1) xs

dropN :: Int -> [a] -> [a]
--Dados un número n y una lista xs, devuelve una lista sin los primeros n elementos de lista
--recibida. Si la lista posee menos de n elementos, se devuelve una lista vacía.
dropN n []     = []
dropN 0 xs     = xs
dropN n (x:xs) = dropN (n-1) xs

splitN :: Int -> [a] -> ([a], [a])
{-Dados un número n y una lista xs, devuelve un par donde la primera componente es la lista
que resulta de aplicar takeN a xs, y la segunda componente el resultado de aplicar
dropN a xs. ¿Conviene utilizar recursión?-}
splitN n xs = (takeN n xs, dropN n xs)

-----------------------------------------------------------------------------------------

--Anexo con ejercicios adicionales
--Ejercicios adicionales para seguir practicando. Defina las siguientes funciones:

maximums :: Ord a => [a] -> a
--Dada una lista devuelve el maximo.
--Parcial en lista vacía
maximums [x]    = x 
maximums (x:xs) = if (x > maximums xs) then x else maximums xs


splitMin :: Ord a => [a] -> (a, [a])
--Devuelve el mínimo y la lista sin él.
splitMin xs = (minimums xs, filtrarElemento (minimums xs) xs)


ordenar :: Ord a => [a] -> [a]
--Dada cualquier lista la devuelve ordenada de menor a mayor.
ordenar [] = []
ordenar xs =  (minimums xs) : ordenar (filtrarElemento (minimums xs) xs)


interseccion :: Eq a => [a] -> [a] -> [a]
--Dada dos listas devuelve otra con los elementos que ambas tengan en común.
interseccion [] []     = []
interseccion xs []     = []
interseccion [] ys     = []
interseccion (x:xs) ys = if (pertenece x ys) then x : interseccion xs ys else interseccion xs ys


diferencia :: Eq a => [a] -> [a] -> [a]
--Dadas dos listas devuelve una lista con los elementos de la primera menos los de la segunda.
diferencia [] []     = []
diferencia xs []     = xs
diferencia [] ys     = []
diferencia xs (y:ys) = if (pertenece y xs) then diferencia (filtrarElemento y xs) ys else y : diferencia xs ys


positivos :: [Int] -> [Int]
--Dada una lista de numeros retorna una lista con todos los numeros positivos
positivos []     = []
positivos (x:xs) = if (x>0) then x : positivos xs else positivos xs


negativos :: [Int] -> [Int]
--Dada una lista de numeros retorna una lista con todos los numeros negativos
negativos []     = []
negativos (x:xs) = if (x<0) then x : negativos xs else negativos xs


particionPorSigno :: [Int] -> ([Int], [Int])
{-Dada una lista xs de enteros devuelva una tupla de listas, donde la primera componente con-
tiene todos aquellos números positivos de xs y la segunda todos aquellos números negativos
de xs. ¿Conviene utilizar recursión? Considere utilizar funciones auxiliares.-}
particionPorSigno xs = (positivos xs, negativos xs)


pares :: [Int] -> [Int]
--Dada una lista de numero retorna una lista con todos los numeros pares de la misma
pares []     = []
pares (x:xs) = if ((mod x 2) == 0) then x : pares xs else pares xs


impares :: [Int] -> [Int]
--Dada una lista de numero retorna una lista con todos los numeros impares de la misma
impares []     = []
impares (x:xs) = if ((mod x 2) == 1) then x : impares xs else impares xs


particionPorParidad :: [Int] -> ([Int], [Int])
{-Dada una lista xs de enteros devuelva una tupla de listas, donde la primera componente
contiene todos aquellos números pares de xs y la segunda todos aquellos números impares
de xs. ¿Conviene utilizar recursión? Considere utilizar funciones auxiliares.-}
particionPorParidad xs = (pares xs, impares xs)


subtails :: [a] -> [[a]]
{-Dada una lista devuelve cada sublista resultante de aplicar tail en cada paso. Ejemplo:
subtails "abc" == ["abc", "bc", "c",""]-}
subtails [] = []
subtails xs = append [xs] (subtails (tails xs))


agrupar :: Eq a => [a] -> [[a]]
{-Dada una lista xs devuelve una lista de listas donde cada sublista contiene elementos conti-
guos iguales de xs. Ejemplo: agrupar "AABCCC" = ["AA","B","CC"]-}
agrupar []     = []
agrupar (x:xs) = if (longitud xs > 0) 
					then if (x == heads xs)
						 then append ([x:[heads xs]]) (agrupar (filtrarElemento x xs))
						 else append [[x]] (agrupar xs) 
					else agrupar xs


esPrefijo :: Eq a => [a] -> [a] -> Bool
--Devuelve True si la primera lista es prefijo de la segunda.
--La primer lista no puede contener mas elementos que la segunda.
esPrefijo [] []			= True
esPrefijo [] ys			= True
esPrefijo (x:xs) (y:ys) = x==y && esPrefijo xs ys


esSufijo :: Eq a => [a] -> [a] -> Bool
--Devuelve True si la primera lista es sufijo de la segunda.
--La primer lista no puede contener mas elementos que la segunda.
esSufijo xs ys = esPrefijo (reversa xs) (reversa ys)
