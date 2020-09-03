--1
somaAngulos :: Float -> Float -> Float -> Bool
somaAngulos x y z =
  if ((x + y + z) == 180 || (x /= 0 && y /= 0 && z /= 0))
    then True
    else False

triangulo :: Float -> Float -> Float -> String
triangulo angulo1 angulo2 angulo3
  | (somaAngulos angulo1 angulo2 angulo3) == False = "Nao_triangulo"
  | angulo1 == angulo2 && angulo2 == angulo3 = "Equilatero"
  | angulo1 == 90 || angulo2 == 90 || angulo3 == 90 = "Retangulo"
  | angulo1 > 90 || angulo2 > 90 || angulo3 > 90 = "Obtuso"
  | otherwise = "Simples"

--2
equacao :: Float -> Float -> Float -> (Float, Float)
equacao a b c =
  if (a /= 0)
    then segnGrau (a, b, c)
    else priGrau (b, c)

--
segnGrau :: (Float, Float, Float) -> (Float, Float)
segnGrau (a, b, c) = ((- b + sqrt ((b * b) -4 * a * c)) / (2 * a), (- b - sqrt ((b * b) -4 * a * c)) / (2 * a))

--
priGrau :: (Float, Float) -> (Float, Float)
priGrau (b, c) = ((- c / b), 0)

--3
type Data = (Int, Int, Int)

defIdade :: Data -> Data -> Int
defIdade (d1, m1, a1) (d2, m2, a2)
  | m1 > m2 = a1 - a2
  | m1 == m2 && d1 > d2 = a1 - a2
  | otherwise = a1 - a2 -1

definirPassagem :: Float -> Data -> Data -> Float
definirPassagem valorPassagem dataAtual dataNascimento
  | (defIdade dataAtual dataNascimento) < 2 = valorPassagem * 0.15
  | (defIdade dataAtual dataNascimento) <= 10 = valorPassagem * 0.4
  | (defIdade dataAtual dataNascimento) >= 70 = valorPassagem * 0.5
  | otherwise = valorPassagem

--4
listaPadrão :: [Integer]
listaPadrão = [1 .. 15]

--A
gera1 :: [Integer]
gera1 = [x ^ 2 | x <- listaPadrão, odd x, x > 4, x < 15]

--B
gera2 :: [(Integer, Integer)]
gera2 = [(x, y) | x <- listaPadrão, x <= 4, y <- [x .. x * 2]]

--C

gera3 :: [[Integer]]
gera3 = [[1 .. y] | y <- listaPadrão, y >= 10, y <= 15]

--D
gera4 :: [(Integer, Integer)]
gera4 = [(x, x + 1) | x <- listaPadrão, odd x]

--E
gera5 :: [Integer]
gera5 = [x + y | (x, y) <- gera4]

--5
--a)
contaNegM2 :: [Int] -> Int
contaNegM2 li = length [x | x <- li, x < 0, x `mod` 2 == 0]

--b)
listaNegM2 :: [Int] -> [Int]
listaNegM2 li = [x | x <- li, x < 0, x `mod` 2 == 0]

--6
distancias :: [(Float, Float)] -> [Float]
distancias [] = []
distancias ((x, y) : tailX) = [raiz | raiz <- (sqrt (x ** 2 + y ** 2)) : (distancias tailX)]

--7
fatores :: Int -> [Int]
fatores n = [x | x <- [1 .. n], n `mod` x == 0]

primos :: Int -> Int -> [Int]
primos ini fim = [n | n <- [ini .. fim], (fatores n) == [1, n]]

--8
mdc2 :: Int -> Int -> Int
mdc2 a b
  | a < b = mdc2 b a
  | b == 0 = a
  | otherwise = mdc2 b (mod a b)

mmc2 :: Int -> Int -> Int
mmc2 x y = (x * y) `div` (mdc2 x y)

mmc :: Int -> Int -> Int -> Int
mmc x y z = mmc2 x (mmc2 y z)

--9
serieX :: Float -> Int -> Float
serieX x n
 | n == 1 = 1 / x
 | even n = (x / fromIntegral(n)) + (serieX x (n-1))
 | otherwise = (fromIntegral(n) / x) + (serieX x (n-1))

--10
auxFiz :: Int -> [String]
auxFiz n
  | n == 0 = []
  | mod n 3 == 0 && mod n 5 == 0 = "FizzBuzz" : auxFiz (n -1)
  | mod n 3 == 0 = "Fizz" : auxFiz (n -1)
  | mod n 5 == 0 = "Buzz" : auxFiz (n -1)
  | otherwise = "No" : auxFiz (n -1)

fizzbuzz :: Int -> [String]
fizzbuzz x = reverse (auxFiz x)

--11
contaOcorrencias1 :: Int -> [Int] -> Int
contaOcorrencias1 x [] = 0
contaOcorrencias1 x (y : tailY)
  | x == y = 1 + (contaOcorrencias1 x tailY)
  | otherwise = contaOcorrencias1 x tailY

conta_ocorrencias :: Int -> Int -> [Int] -> (Int, Int)
conta_ocorrencias valor1 valor2 lista1 = (contaOcorrencias1 valor1 lista1, contaOcorrencias1 valor2 lista1)

--12
unica_ocorrencia :: Int -> [Int] -> Bool
unica_ocorrencia x lista =
  if ((contaOcorrencias1 x lista) == 1)
    then True
    else False

--13
intercala x [] = x
intercala [] x = x
intercala (a : xs) (b : ys) = a : b : intercala xs ys

-- 14 Feito em monitoria
type Contato = (String, String, String, String)

contatos :: [Contato]
contatos =
  [ ("Nome1", "Ender1", "Tel1", "Email1"),
    ("Nome2", "Ender2", "Tel2", "Email2"),
    ("Nome3", "Ender3", "Tel3", "Email3")
  ]

encontraContatoAux :: [Contato] -> String -> String
encontraContatoAux [] email = "Email nao encontrado"
encontraContatoAux ((nome, _, _, emailInput) : outrosElementos) email
  | emailInput == email = nome
  | otherwise = encontraContatoAux outrosElementos email

encontraContato :: String -> String
encontraContato emailEncontrar = encontraContatoAux contatos emailEncontrar

--15
type Pessoa = (String, Float, Int, Char)

pessoas :: [Pessoa]
pessoas =
  [ ("João", 1.85, 26, 'C'),
    ("Maria", 1.55, 62, 'S'),
    ("Jose", 1.78, 42, 'C'),
    ("Paulo", 1.93, 25, 'S'),
    ("Clara", 1.70, 33, 'C'),
    ("Bob", 1.45, 21, 'C'),
    ("Rosana", 1.58, 39, 'S'),
    ("Daniel", 1.74, 72, 'S'),
    ("Jocileide", 1.69, 18, 'S')
  ]

--Altura media
mediaAltura :: [Pessoa] -> Float
mediaAltura lista = (somaAltura lista) / fromIntegral (length lista) :: Float

somaAltura :: [Pessoa] -> Float
somaAltura [] = 0
somaAltura ((nome, altura, idade, estado) : tail) = altura + (somaAltura tail)

---Idade da pessoa mais nova

criaListaComIdades :: [Pessoa] -> [Int]
criaListaComIdades [] = []
criaListaComIdades ((nome, altura, idade, estado) : tail) = [id | id <- idade : (criaListaComIdades tail)]

idMaisNova :: [Pessoa] -> Int
idMaisNova lista = minimum (criaListaComIdades lista)

--Estado civil Mais velho
idMaisVelha :: [Pessoa] -> Int
idMaisVelha lista = maximum (criaListaComIdades lista)

encontraMaisVelhoAux :: [Pessoa] -> Int -> (String, Char)
encontraMaisVelhoAux [] idadeIn = ("", ' ')
encontraMaisVelhoAux ((nome, _, idade, estadoCiv) : outrosElementos) idadeIn
  | idadeIn == idade = (nome, estadoCiv)
  | otherwise = encontraMaisVelhoAux outrosElementos idadeIn

encontraMaisVelho :: [Pessoa] -> (String, Char)
encontraMaisVelho lista = encontraMaisVelhoAux pessoas (idMaisVelha lista)

--Pessoas com mais de 50
pessoasMelhorIdade lista =
  [(nome, altura, idade, estado) | (nome, altura, idade, estado) <- lista, idade >= 50]

--casados com mais de 50
pessoasCasadasMaiorX lista x =
  [(nome, altura, idade, estado) | (nome, altura, idade, estado) <- lista, idade >= x, estado == 'C']

--16
insere_ord x [] = [x]
insere_ord x (y : ys)
  | x <= y = (x : y : ys)
  | otherwise = y : (insere_ord x ys)

--17
reverte [] = []
reverte (x : xs) = (reverte xs) ++ [x]

--18
membro x [] = False
membro x (head : tail)
  | x == head = True
  | otherwise = membro x tail

sem_repetidos [] = []
sem_repetidos (x : tail)
  | membro x tail = sem_repetidos (tail)
  | otherwise = x : sem_repetidos (tail)

--19
disponiveis :: [Int]
disponiveis = [1, 2, 5, 10, 20, 50, 100]

notasTroco :: Int -> [[Int]]
notasTroco 0 = [[]]
notasTroco valor = [head : tail | head <- disponiveis, head <= valor, tail <- notasTroco (valor - head)]