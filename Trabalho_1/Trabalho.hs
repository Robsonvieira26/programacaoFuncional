--1
somaAngulos :: Float -> Float -> Float -> Bool
somaAngulos x y z =
  if ((x + y + z) == 180)
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

