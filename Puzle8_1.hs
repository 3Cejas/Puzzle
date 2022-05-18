-- AUTORES: 
-- DAVID VIÑAS MORALES X15M043
-- MARIO LÓPEZ CABELLO Z17M037

{-# LANGUAGE BlockArguments #-}
module Puzle8_1 where
import Test.QuickCheck
import Data.List



---------PARAMETRO DEL JUEGO
dimension = 3

--------------- TIPOS:  NO TOCAR NADA
type Posicion = (Int,Int)

type Movimiento = (Int,Int)

type Solucion = [Movimiento]

--Tablero es una tupla posicion del hueco, lista de pares
newtype Tablero = T (Posicion,Posicion->Maybe Int)

--Lista [(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)]
posicionesValidas::[Posicion]
posicionesValidas = [ (x, y) | x <- [1..dimension] , y <- [1..dimension] ]

--Función que devuelve el tablero objetivo
posiciona:: Posicion -> Maybe Int
posiciona (x, y) = if x == dimension && dimension  == y
                      then Nothing
                    else Just ((x-1)*dimension + y `mod` (dimension+1))

-- T (Posicion,(Posicion->Maybe Int))                 
tableroFinal::Tablero
tableroFinal = T((dimension,dimension), posiciona)

--Dados un tablero y un Maybe Int (el Int entre 1 y 8) devuelve su posicion en el tablero

posElem:: Tablero -> Maybe Int -> Posicion
posElem (T(_,f)) n =  posicionesValidas !! castMaybe x   where x = posElemAux(map f posicionesValidas) n


posElemAux :: [Maybe Int]-> Maybe Int -> Maybe Int
posElemAux [] _  =   Nothing
posElemAux  (x:xs)   e
  |x==e  = Just 0
  |otherwise  = posElemAux xs e >>= (\pos -> Just (pos+1))

castMaybe:: Num a => Maybe a -> a
castMaybe(Just x) = x
castMaybe(Nothing) = 0

--Dados un tablero y una posicion devuelve el elemento en la posicion
elemInPos::Tablero -> Posicion -> Maybe Int
elemInPos (T (_, f)) = f

--Igualdad entre tableros - Atencion,la igualdad entre funciones no esta predefinida
instance Eq Tablero where
  T(v1,f1)==T(v2,f2) =  [f1 pos | pos <- posicionesValidas] == [f2 pos | pos <- posicionesValidas]

--Muestra tablero como lista de Maybe Int recorriendo tablero de izq. a dcha. y de arriba abajo.  
instance Show Tablero where
  show (T(_,f)) = show (map f posicionesValidas)

--Pasa de una lista de elementos Maybe Int (resultado de un show de tablero) a un Tablero
list2tab::[Maybe Int] -> Tablero
list2tab x = T( posicionesValidas !! castMaybe(elemIndex Nothing x)  ,\p -> x !! castMaybe(elemIndex p posicionesValidas))


--Dado un movimiento y un tablero devuelve el resultante de ejecutar ese movimiento
actualizaTab :: Movimiento -> Tablero-> Tablero
actualizaTab m (T(p,f)) = T(m, \r-> if r == m
                                        then elemInPos (T(p,f)) p
                                      else if r == p
                                        then elemInPos (T(p,f)) m
                                      else f r
                                      )


isMember :: Eq a => a -> [a] -> Bool
isMember n [] = False
isMember n (x:xs)
    | n == x = True
    | otherwise = isMember n xs

--Posiciones del tablero adyacentes a la del hueco
movPosibles::Tablero ->[Movimiento]
movPosibles (T((x,y),f)) = [(x1 ,y1) | (x1, y1) <- posicionesValidas , abs(x1-x)+ abs(y1 -y) == 1 ]


--Dados tablero y lista tableros visitados,devuelve tableros no visitados alcanzables en 1 movimiento  
nextTabs::Tablero->[Tablero]->[Tablero]
nextTabs (T(p,f)) l= [t | t <- map (\h -> actualizaTab h (T(p,f)) ) (movPosibles (T (p, f))), not (isMember t l)]

-----------------------
--Dice si un tablero tiene solucion en un maximo de 15 movimientos. Evitad bucles infinitos.
--Si la función es correcta al evaluar  >map (tieneSol.list2tab) testConSol   debe devolver una lista
--en la que todo los elementos son True, y al evaluar  >map (tieneSol.list2tab) testSinSol  una lista
--en la que todos son False.



tieneSol::Tablero -> Bool
tieneSol tab = tieneSolAux tab 0 []
  where
    tieneSolAux :: Tablero -> Int ->[Tablero] -> Bool
    tieneSolAux tableroFinal n  list= True 
    tieneSolAux tab 12  list = False
    tieneSolAux tab n  list= foldr (||) False (map (\h-> (tieneSolAux h (n+1) (list++[h]))) (nextTabs tab list))

---------------------------
--Generador de tableros con solución en un numero aleatorio de movimientos acotado por maxLength
maxLength = 14

genConSol::Gen Tablero
genConSol= oneof (map return (map list2tab tabsConSol))


-------------------------------------
--Debeis definir prettyprintAlumno
--Probadla con tableros de cualquiera de las dos listas de tableros (tabsConSol o tabsSinSol)

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where
    (first,rest) = splitAt n list

prettyprint :: Tablero -> IO ()
prettyprint tab = putStrLn $ prettyprintAlumno tab
prettyprintAlumno :: Tablero -> String
prettyprintAlumno (T(p,f)) = concat((map (foldr ((addprint) . show) "|\n") (splitEvery dimension (map (castMaybe . f) posicionesValidas))))


addprint :: String -> String -> String
addprint s1 s2 = if s1 == "0" then "|"++ " " ++s2 
                  else "|"++s1 ++ s2 

--Lista de todas las soluciones  posibles de longitud maxima 15
dameTodasSol :: Tablero -> [Solucion]
dameTodasSol = undefined

--Definidla para poder comprobar que una solucion es correcta
comprobarSol:: Tablero -> Solucion -> Bool
comprobarSol = undefined

--Tableros que tienen al menos una solucion en 15 o menos movimientos
tabsConSol::[[Maybe Int]]
tabsConSol=[[Just 1,Just 2,Just 3,Just 4,Nothing,Just 8,Just 7,Just 6,Just 5],[Just 5,Just 3,Just 6,Just 2,Just 1,Just 8,Nothing,Just 4,Just 7],[Just 1,Just 2,Just 3,Just 5,Nothing,Just 7,Just 8,Just 6,Just 4],[Nothing,Just 3,Just 5,Just 1,Just 7,Just 2,Just 8,Just 4,Just 6],[Just 3,Just 8,Just 5,Just 1,Just 2,Just 6,Just 4,Just 7,Nothing],[Just 2,Just 5,Just 3,Just 1,Nothing,Just 6,Just 7,Just 8,Just 4],[Just 1,Just 3,Just 8,Just 7,Just 4,Just 5,Nothing,Just 2,Just 6],[Just 4,Just 1,Just 2,Just 7,Nothing,Just 6,Just 8,Just 3,Just 5],[Just 1,Just 2,Just 6,Just 4,Just 5,Just 8,Nothing,Just 7,Just 3],[Just 7,Just 4,Just 1,Just 2,Nothing,Just 3,Just 8,Just 6,Just 5],[Just 7,Just 1,Just 2,Just 8,Nothing,Just 3,Just 4,Just 5,Just 6]]

tabsSinSol::[[Maybe Int]]
tabsSinSol=[[Just 1,Just 2,Just 4,Just 3,Nothing,Just 8,Just 7,Just 6,Just 5],[Just 2,Just 6,Just 3,Just 1,Nothing,Just 8,Just 7,Just 4,Just 5],[Nothing,Just 1,Just 2,Just 7,Just 4,Just 6,Just 5,Just 8,Just 3],[Just 3,Just 5,Just 6,Just 2,Just 1,Just 8,Nothing,Just 4,Just 7],[Just 8,Just 2,Nothing,Just 1,Just 3,Just 6,Just 4,Just 7,Just 5],[Nothing,Just 3,Just 2,Just 4,Just 8,Just 1,Just 5,Just 7,Just 6],[Nothing,Just 2,Just 4,Just 5,Just 3,Just 6,Just 1,Just 7,Just 8],[Just 4,Just 7,Just 1,Just 5,Just 2,Just 3,Nothing,Just 8,Just 6],[Just 2,Just 1,Just 3,Just 5,Nothing,Just 7,Just 8,Just 6,Just 4],[Nothing,Just 5,Just 3,Just 1,Just 7,Just 2,Just 8,Just 4,Just 6]]

--Para las pruebas de dameTodasSol
testTodasSol :: [([Maybe Integer], [[(Integer, Integer)]])]
testTodasSol= [([Just 1,Just 2,Just 3,Just 4,Nothing,Just 8,Just 7,Just 6,Just 5],[[(1,2),(1,1),(2,1),(2,2),(2,3),(3,3),(3,2),(2,2),(2,1),(1,1),(1,2),(2,2),(2,3),(3,3)],[(1,2),(1,1),(2,1),(2,2),(3,2),(3,3),(2,3),(2,2),(2,1),(1,1),(1,2),(2,2),(3,2),(3,3)],[(2,1),(1,1),(1,2),(2,2),(2,3),(3,3),(3,2),(2,2),(1,2),(1,1),(2,1),(2,2),(2,3),(3,3)],[(2,1),(1,1),(1,2),(2,2),(3,2),(3,3),(2,3),(2,2),(1,2),(1,1),(2,1),(2,2),(3,2),(3,3)],[(2,3),(3,3),(3,2),(2,2),(2,3),(3,3)],[(3,2),(3,3),(2,3),(2,2),(3,2),(3,3)]]),([Just 5,Just 3,Just 6,Just 2,Just 1,Just 8,Nothing,Just 4,Just 7],[[(3,2),(3,3),(2,3),(1,3),(1,2),(1,1),(2,1),(2,2),(1,2),(1,1),(2,1),(3,1),(3,2),(3,3)],[(3,2),(3,3),(2,3),(1,3),(1,2),(2,2),(2,1),(1,1),(1,2),(2,2),(2,1),(3,1),(3,2),(3,3)]]),([Just 1,Just 2,Just 3,Just 5,Nothing,Just 7,Just 8,Just 6,Just 4],[[(2,3),(3,3),(3,2),(3,1),(2,1),(2,2),(2,3),(3,3),(3,2),(3,1),(2,1),(2,2),(3,2),(3,3)]]),([Nothing,Just 3,Just 5,Just 1,Just 7,Just 2,Just 8,Just 4,Just 6],[[(2,1),(2,2),(1,2),(1,3),(2,3),(2,2),(3,2),(3,1),(2,1),(2,2),(1,2),(1,3),(2,3),(3,3)],[(2,1),(2,2),(2,3),(1,3),(1,2),(2,2),(3,2),(3,1),(2,1),(2,2),(2,3),(3,3)],[(2,1),(2,2),(3,2),(3,1),(2,1),(2,2),(1,2),(1,3),(2,3),(2,2),(1,2),(1,3),(2,3),(3,3)],[(2,1),(2,2),(3,2),(3,1),(2,1),(2,2),(2,3),(1,3),(1,2),(2,2),(2,3),(3,3)],[(2,1),(3,1),(3,2),(2,2),(2,1),(3,1),(3,2),(2,2),(2,3),(1,3),(1,2),(2,2),(2,3),(3,3)],[(2,1),(3,1),(3,2),(2,2),(2,3),(1,3),(1,2),(2,2),(2,1),(3,1),(3,2),(2,2),(2,3),(3,3)]]),([Just 3,Just 8,Just 5,Just 1,Just 2,Just 6,Just 4,Just 7,Nothing],[[(2,3),(2,2),(1,2),(1,1),(2,1),(3,1),(3,2),(2,2),(2,3),(1,3),(1,2),(2,2),(2,3),(3,3)]]),([Just 2,Just 5,Just 3,Just 1,Nothing,Just 6,Just 7,Just 8,Just 4],[[(1,2),(1,1),(2,1),(3,1),(3,2),(3,3),(2,3),(2,2),(3,2),(3,1),(2,1),(2,2),(2,3),(3,3)],[(1,2),(1,3),(2,3),(3,3),(3,2),(2,2),(2,3),(1,3),(1,2),(1,1),(2,1),(2,2),(3,2),(3,3)]]),([Just 1,Just 3,Just 8,Just 7,Just 4,Just 5,Nothing,Just 2,Just 6],[[(2,1),(2,2),(3,2),(3,3),(2,3),(1,3),(1,2),(2,2),(2,3),(3,3),(3,2),(2,2),(2,3),(3,3)],[(2,1),(2,2),(3,2),(3,3),(2,3),(1,3),(1,2),(2,2),(3,2),(3,3),(2,3),(2,2),(3,2),(3,3)]]),([Just 4,Just 1,Just 2,Just 7,Nothing,Just 6,Just 8,Just 3,Just 5],[[(2,3),(3,3),(3,2),(2,2),(2,3),(3,3),(3,2),(3,1),(2,1),(1,1),(1,2),(1,3),(2,3),(3,3)],[(3,2),(3,3),(2,3),(2,2),(3,2),(3,1),(2,1),(1,1),(1,2),(1,3),(2,3),(3,3)]]),([Just 1,Just 2,Just 6,Just 4,Just 5,Just 8,Nothing,Just 7,Just 3],[[(3,2),(3,3),(2,3),(1,3),(1,2),(2,2),(3,2),(3,3),(2,3),(2,2),(1,2),(1,3),(2,3),(3,3)]]),([Just 7,Just 4,Just 1,Just 2,Nothing,Just 3,Just 8,Just 6,Just 5],[[(2,1),(1,1),(1,2),(1,3),(2,3),(3,3),(3,2),(3,1),(2,1),(1,1),(1,2),(2,2),(2,3),(3,3)]]),([Just 7,Just 1,Just 2,Just 8,Nothing,Just 3,Just 4,Just 5,Just 6],[[(3,2),(3,1),(2,1),(1,1),(1,2),(1,3),(2,3),(2,2),(3,2),(3,1),(2,1),(2,2),(2,3),(3,3)],[(3,2),(3,1),(2,1),(1,1),(1,2),(2,2),(3,2),(3,1),(2,1),(2,2),(1,2),(1,3),(2,3),(3,3)]])]

check::[([Maybe Int],[Solucion])]->String
check [] = "Test dameTodasSol superado"
check ((arg1, sol):xs) =
 if esPermutacion resultalumno sol
 then check xs
 else "Fallo en test dameTodasSol, el resultado esperado para el argumento " ++
      show arg1 ++  ", es " ++ show sol ++ ", " ++
      "pero el resultado obtenido es " ++ show resultalumno
      where resultalumno = dameTodasSol $ list2tab arg1

esPermutacion::Eq a => [a]->[a]->Bool
esPermutacion [] [] = True
esPermutacion (x:xs) ys = elem x ys && esPermutacion xs (delete x ys)
esPermutacion _ _ = False
----------------------                 



