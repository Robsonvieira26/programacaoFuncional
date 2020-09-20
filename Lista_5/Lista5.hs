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
prodIntervalo m n = if (m >= n) then exp1 else exp2
  where
    exp1 = n
    exp2 = (m * (prodIntervalo (m + 1) n))

fatInter :: Int -> Int
fatInter n = exp1
  where
    exp1 = prodIntervalo 1 n

-- ==============================================================
-- Exercício 2:
-- A)
bissextoLet :: Int -> Bool
bissextoLet ano =
  let exp1 = (mod ano 400 == 0)
      exp2 = (mod ano 4 == 0)
      exp3 = (mod ano 100 /= 0)
   in exp1 || (exp2 && exp3)

type DataLet = (Int, Int, Int)

validaLet :: DataLet -> Bool
validaLet (dia, mes, ano) =
  let exp1 = dia >= 1 && dia <= 31 && (mes == 1 || mes == 3 || mes == 5 || mes == 7 || mes == 8 || mes == 10 || mes == 12)
      exp2 = dia >= 1 && dia <= 30 && (mes == 4 || mes == 6 || mes == 9 || mes == 11)
      exp3 = dia >= 1 && dia <= 28 && mes == 2 && not (bissextoLet ano)
      exp4 = dia >= 1 && dia <= 29 && mes == 2 && (bissextoLet ano)
   in exp1 || exp2 || exp3 || exp4

--B)
bissextosLet :: [Int] -> [Int]
bissextosLet lista =
  let exp1 = [x | x <- lista, bissextoLet x]
   in exp1

--C)
type EmprestimoLet = (String, String, DataLet, DataLet, String)

type EmprestimosLet = [EmprestimoLet]

bdEmprestimoLet :: EmprestimosLet
bdEmprestimoLet =
  [ ("H123C9", "BSI945", (12, 9, 2009), (20, 09, 2009), "aberto"),
    ("L433C5", "BCC021", (01, 9, 2009), (10, 09, 2009), "encerrado"),
    ("M654C3", "BCC008", (04, 9, 2009), (15, 09, 2009), "aberto")
  ]

procedeLet :: DataLet -> DataLet -> Bool
procedeLet (dia, mes, ano) (dia2, mes2, ano2) =
  let exp1 = not (validaLet (dia, mes, ano)) || not (validaLet (dia2, mes2, ano2))
      exp2 = ano > ano2
      exp3 = ano == ano2 && mes > mes2
      exp4 = ano == ano2 && mes == mes && dia > dia2
   in not (exp1 || exp2 || exp3 || exp4)

emprestimoEmDiaLet :: DataLet -> EmprestimoLet -> Bool
emprestimoEmDiaLet dataAtual (codLivro, codAluno, dataEmprest, dataDevo, status) =
  let exp1 = procedeLet dataAtual dataDevo
   in exp1

atrasadosLet :: EmprestimosLet -> DataLet -> EmprestimosLet
atrasadosLet listaEmprestimos dataAtual =
  let exp1 = [x | x <- listaEmprestimos, not (emprestimoEmDiaLet dataAtual x)]
   in exp1

--D)
passoLet :: (Int, Int) -> (Int, Int)
passoLet (x, y) =
  let exp1 = (y, x + y)
   in exp1

fibo2Let :: Int -> (Int, Int)
fibo2Let 0 = (0, 1)
fibo2Let n =
  let exp1 = passoLet (fibo2Let (n -1))
   in exp1

--E)
prodIntervaloLet :: Int -> Int -> Int
prodIntervaloLet m n =
  let exp1 =
        if (m >= n)
          then n
          else (m * (prodIntervalo (m + 1) n))
   in exp1

fatInterLet :: Int -> Int
fatInterLet n =
  let exp1 = prodIntervalo 1 n
   in exp1

-- 3) Arquivo .txt
-- 4) Arquivo .png
-- 5)
--a) (\x -> \y -> y) ((\z -> z) (\z -> z)) (\w -> w) 5
--b) ((\f-> (\x -> f (f x))) (\y -> (y * y ))) 3
--TODO:conferir o erro c) ((\f-> (\x-> f(f x))) (\y-> (+ y y))) 5
--TODO:conferir o erro d) ((\x-> (\y-> + x y) 5) ((\y-> - y 3) 7))
--e) (((\f-> (\x-> f(f(f x)))) (\y-> (y * y))) 2)
--TODO:conferir o erro f) (\x-> \y-> + x ((\x-> - x 3) y)) 5 6