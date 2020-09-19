-- Exercício 1:
-- A)
bissexto :: Int -> Bool
bissexto ano = exp1 || (exp2 && exp3)
  where
    exp1 = (mod ano 400 == 0)
    exp2 = (mod ano 4 == 0)
    exp3 = (mod ano 100 /= 0)

type Data = (Int, Int, Int)

valida :: Data -> Bool
valida (dia, mes, ano) = exp1 || exp2 || exp3 || exp4
  where
    exp1 = dia >= 1 && dia <= 31 && (mes == 1 || mes == 3 || mes == 5 || mes == 7 || mes == 8 || mes == 10 || mes == 12)
    exp2 = dia >= 1 && dia <= 30 && (mes == 4 || mes == 6 || mes == 9 || mes == 11)
    exp3 = dia >= 1 && dia <= 28 && mes == 2 && not (bissexto ano)
    exp4 = dia >= 1 && dia <= 29 && mes == 2 && (bissexto ano)

--B)
bissextos :: [Int] -> [Int]
bissextos lista = exp1
  where
    exp1 = [x | x <- lista, bissexto x]

--C)
type Emprestimo = (String, String, Data, Data, String)

type Emprestimos = [Emprestimo]

bdEmprestimo :: Emprestimos
bdEmprestimo =
  [ ("H123C9", "BSI945", (12, 9, 2009), (20, 09, 2009), "aberto"),
    ("L433C5", "BCC021", (01, 9, 2009), (10, 09, 2009), "encerrado"),
    ("M654C3", "BCC008", (04, 9, 2009), (15, 09, 2009), "aberto")
  ]

procede :: Data -> Data -> Bool
procede (dia, mes, ano) (dia2, mes2, ano2) = not (exp1 || exp2 || exp3 || exp4)
  where
    exp1 = not (valida (dia, mes, ano)) || not (valida (dia2, mes2, ano2))
    exp2 = ano > ano2
    exp3 = ano == ano2 && mes > mes2
    exp4 = ano == ano2 && mes == mes && dia > dia2

emprestimoEmDia :: Data -> Emprestimo -> Bool
emprestimoEmDia dataAtual (codLivro, codAluno, dataEmprest, dataDevo, status) = exp1
  where
    exp1 = procede dataAtual dataDevo

atrasados :: Emprestimos -> Data -> Emprestimos
atrasados listaEmprestimos dataAtual = exp1
  where
    exp1 = [x | x <- listaEmprestimos, not (emprestimoEmDia dataAtual x)]

--D)
passo :: (Int, Int) -> (Int, Int)
passo (x, y) = exp1
  where
    exp1 = (y, x + y)

fibo2 :: Int -> (Int, Int)
fibo2 0 = (0, 1)
fibo2 n = exp1
  where
    exp1 = passo (fibo2 (n -1))

--E)
prodIntervalo :: Int -> Int -> Int
prodIntervalo m n
  | m == n = n
  | otherwise = n * prodIntervalo m (n -1)

fatProdIntervalo :: Int -> Int
fatProdIntervalo 1 = 1
fatProdIntervalo n = exp1
  where
    exp1 = n * fatProdIntervalo (n -1)