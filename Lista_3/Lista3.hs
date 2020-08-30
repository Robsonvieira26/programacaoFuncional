--Exercício 1: A
myOr :: Bool -> Bool -> Bool
myOr True True = True
myOr True False = True
myOr False True = True
myOr False False = False

myOr2 :: Bool -> Bool -> Bool
myOr2 False False = False
myOr2 True _ = True

myOr3 :: Bool -> Bool -> Bool
myOr3 False b = b
myOr3 True _ = True

--B
myOr4 :: Bool -> Bool -> Bool
myOr4 a b =
  if (a == False && b == False)
    then False
    else True

myOr5 :: Bool -> Bool -> Bool
myOr5 a b =
  if (a /= b)
    then True
    else
      if (a == b && b == False)
        then False
        else True

--Exercício 2
quadrado :: Float -> Float
quadrado x = x * x

distanciaPts :: (Float, Float) -> (Float, Float) -> Float
distanciaPts (x1, y1) (x2, y2) = sqrt (quadrado (x2 - x1) + quadrado (y2 - y1))

--Exercício 4
fatorial :: Int -> Int
fatorial 0 = 1
fatorial n = n * fatorial (n -1)

--Exercício 5
fibo :: Int -> Int
fibo n
  | n == 0 = 1
  | n == 1 = 1
  | otherwise = fibo (n -2) + fibo (n -1)

--Exercício 6
n_tri :: Int -> Int
n_tri n
  | n == 0 = 0
  | n == 1 = 1
  | n == 2 = 3
  | n == 3 = 6
  | otherwise = n_tri (n -3) + n_tri (n -2) + n_tri (n -1)

--Exercício 7
passo :: (Int, Int) -> (Int, Int)
passo (x, y) = (y, x + y)

auxiliarFibo :: Int -> (Int,Int)
auxiliarFibo 1 = (1, 1)
auxiliarFibo n = passo (auxiliarFibo (n-1))

fibo2 :: Int -> Int
fibo2 n = do
    let (x, y) = auxiliarFibo n
    x



--Exercício 8
potencia2 :: Int -> Int
potencia2 expo
  | expo == 0 = 1
  | otherwise = (2 * potencia2 (expo -1))

--Exercício 9
--A
prodIntervalo :: Int -> Int -> Int
prodIntervalo m n
  | m == n = n
  | otherwise = n * prodIntervalo m (n -1)
--B
fiboProdIntervalo :: Int -> Int
fiboProdIntervalo  n
  | 1 == n = n
  | otherwise = n * fiboProdIntervalo  (n -1)

--Exercício 11
resto_div :: Int -> Int -> Int
resto_div dividendo divisor =
  if dividendo == 0 || divisor == 1
    then 0
    else
      if dividendo < divisor
        then dividendo
        else resto_div (dividendo - divisor) divisor

div_inteira :: Int -> Int -> Int
div_inteira dividendo divisor =
  if (dividendo < divisor)
    then 0
    else (div_inteira (dividendo - divisor) divisor) + 1

--Exercício 12
--Com guardas:
mdcGuardas :: (Int, Int) -> Int
mdcGuardas (m, n)
  | n == 0 = m
  | otherwise = mdcGuardas (n, (mod m n))

--Casamento de Padrões
mdc :: (Int, Int) -> Int
mdc (m, 0) = m
mdc (m, n) = mdc (n, (mod m n))

--Exercício 13
--Com Guardas
binomialGuardas :: (Int, Int) -> Int
binomialGuardas (n, k)
  | k == 0 = 1
  | k == n = 1
  | otherwise = binomialGuardas (n -1, k) + binomialGuardas (n -1, k -1)

--Casamento de Padrões
binomial :: (Int, Int) -> Int
binomial (n, 0) = 1
binomial (n, k) =
  if (k == n)
    then 1
    else binomial (n -1, k) + binomial (n -1, k -1)

--Exercício 15
--A
enumeracao :: Int -> Int -> [Int]
enumeracao a b 
  | a==b = a:[]
  |a>b=[]
  |otherwise=[a..b]