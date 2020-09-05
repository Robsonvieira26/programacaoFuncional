--1)
lst1 = [x * 2 | x <- [1 .. 10], x * 2 >= 12]

lst2 = [x | x <- [50 .. 100], mod x 7 == 3]

lst3 = [x | x <- [10 .. 20], x /= 13, x /= 15, x /= 19]

lst4 = [(x, y) | x <- [1 .. 4], y <- [x .. 5]]

--2)
quadrados :: Int -> Int -> [Int]
quadrados ini fim = [x ^ 2 | x <- [ini .. fim]]

--3)
seleciona_impares :: [Int] -> [Int]
seleciona_impares lista = [x | x <- lista, odd x]

--4)
tabuada x = [x * cont | cont <- [1 .. 10]]

--5)
bissexto :: Int -> Bool
bissexto ano
  | (mod ano 400 == 0) = True
  | (mod ano 4 == 0) && (mod ano 100 /= 0) = True
  | otherwise = False

bissextos :: [Int] -> [Int]
bissextos lista = [x | x <- lista, bissexto x]

--TODO: 6)sublistas

--7)
type Data = (Int, Int, Int)

type Emprestimo = (String, String, Data, Data, String)

type Emprestimos = [Emprestimo]

bdEmprestimo :: Emprestimos
bdEmprestimo =
  [ ("H123C9", "BSI945", (12, 9, 2009), (20, 09, 2009), "aberto"),
    ("L433C5", "BCC021", (01, 9, 2009), (10, 09, 2009), "encerrado"),
    ("M654C3", "BCC008", (04, 9, 2009), (15, 09, 2009), "aberto")
  ]

valida :: Data -> Bool
valida (dia, mes, ano)
  | dia >= 1 && dia <= 31 && (mes == 1 || mes == 3 || mes == 5 || mes == 7 || mes == 8 || mes == 10 || mes == 12) = True
  | dia >= 1 && dia <= 30 && (mes == 4 || mes == 6 || mes == 9 || mes == 11) = True
  | dia >= 1 && dia <= 28 && mes == 2 && not (bissexto ano) = True
  | dia >= 1 && dia <= 29 && mes == 2 && (bissexto ano) = True
  | otherwise = False

procede :: Data -> Data -> Bool
procede (dia, mes, ano) (dia2, mes2, ano2)
  | not (valida (dia, mes, ano)) || not (valida (dia2, mes2, ano2)) = False
  | ano > ano2 = False
  | ano == ano2 && mes > mes2 = False
  | ano == ano2 && mes == mes && dia > dia2 = False
  | otherwise = True

emprestimoEmDia :: Data -> Emprestimo -> Bool
emprestimoEmDia dataAtual (codLivro, codAluno, dataEmprest, dataDevo, status) =
  procede dataAtual dataDevo

atrasados :: Emprestimos -> Data -> Emprestimos
atrasados listaEmprestimos dataAtual = [x | x <- listaEmprestimos, not (emprestimoEmDia dataAtual x)]

--8)
npares :: [Int] -> Int
npares lista = length [x | x <- lista, even x]

--9)
produtorio::[Int] -> Int
produtorio [] = 1
produtorio (head : tail) = head * produtorio tail

--todo 10) unzip

--11)
tamanho :: [a] -> Int
tamanho [] = 0
tamanho (head : tail) = 1 + tamanho tail

--12)
membro :: Eq t => t -> [t] -> Bool
membro x [] = False
membro x (head : tail)
  | x == head = True
  | otherwise = membro x tail

uniaoNRec :: [Int] -> [Int] -> [Int]
uniaoNRec lista1 lista2 = [x | x <- lista1] ++ [y | y <- lista2, not (membro y lista1)]

--13)
uniaoRec2 :: [Int] -> [Int] -> [Int]
uniaoRec2 (a : as) (x : xs)
  | (a == x) = uniaoRec2 as xs
  | otherwise = a : as ++ x : xs
