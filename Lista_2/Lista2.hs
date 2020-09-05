--Exercício 1:
dobro :: Float -> Float
dobro valor = valor * 2
quadruplo :: Float -> Float
quadruplo valor = (dobro valor ) + (dobro valor )
hipotenusa :: Float -> Float -> Float
hipotenusa catetoOposto catetoAdjacente = sqrt ((catetoOposto**2) + (catetoAdjacente**2))
dist :: Float ->Float ->Float ->Float->  Float
dist x1 y1 x2 y2 = sqrt( ((x2-x1) ** 2) + ((y2-y1) ** 2) )

--Exercício 3:
conv :: Float -> (Float,Float,Float)
conv qtdReais = (qtdReais,(qtdReais*3.96),(qtdReais*4.45))

--Exercício 4:
bissexto :: Int -> Bool
bissexto ano  | (mod ano 400 == 0) = True
              | (mod ano 4 == 0) && (mod ano 100 /= 0) = True
              | otherwise = False 

-- Exercício 5:
type Data = (Int, Int, Int)
bissexto2 :: Data -> Bool
bissexto2 (dia,mes,ano) | (mod ano 400 == 0) = True
                        | (mod ano 4 == 0) && (mod ano 100 /= 0) = True
                        | otherwise = False 

-- Exercício 6:
valida :: Data -> Bool
valida (dia,mes,ano)
  | dia >= 1 && dia <= 31 && (mes == 1 || mes == 3 || mes == 5 || mes == 7 || mes == 8 || mes == 10 || mes == 12) = True
  | dia >= 1 && dia <= 30 && ( mes == 4 || mes == 6 || mes == 9 || mes == 11 ) = True
  | dia >= 1 && dia <= 28 && mes == 2 && not (bissexto ano) = True
  | dia >= 1 && dia <= 29 && mes == 2 && (bissexto ano) = True
  | otherwise = False

-- Exercício 7:
procede :: Data -> Data -> Bool
procede (dia,mes,ano) (dia2,mes2,ano2)
        | not( valida (dia,mes,ano)) ||not (valida (dia2,mes2,ano2)) = False
        | ano > ano2 = False
        | ano == ano2 && mes > mes2 = False
        | ano == ano2 && mes == mes && dia > dia2 = False
        |otherwise = True

-- Exercício 8:
type Livro = (String,String,String,String,Int)
type Aluno = (String,String,String,String)
type Emprestimo = (String, String, Data, Data, String)

-- Exercício 9:
emprestimoEmDia :: Data -> Emprestimo -> Bool
emprestimoEmDia dataAtual (codLivro,codAluno, dataEmprest, dataDevo, status)
            = procede dataAtual dataDevo
