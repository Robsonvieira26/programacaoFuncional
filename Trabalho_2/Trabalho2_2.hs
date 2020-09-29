--TODO: Ex6
data Exp a
  = Val a -- um numero
  | Add (Exp a) (Exp a) -- soma de duas expressoes
  | Sub (Exp a) (Exp a) --subtração

avalia :: Num a => Exp a -> a
avalia (Val x) = x
avalia (Add exp1 exp2) = (avalia exp1) + (avalia exp2)
avalia (Sub exp1 exp2) = (avalia exp1) - (avalia exp2)

--Ex7

data Hora = PM Int Int | AM Int Int deriving (Eq, Show, Ord)

validaHora :: Int -> Bool
validaHora h
  | h >= 0 && h <= 11 = True
  | otherwise = False

validaMin :: Int -> Bool
validaMin m
  | m >= 0 && m <= 59 = True
  | otherwise = False

horasDecorridas :: Hora -> Int
horasDecorridas (AM hora min)
  | validaHora (hora) == True && validaMin (min) == True = hora
  | otherwise = -1
horasDecorridas (PM hora min)
  | validaHora (hora) == True && validaMin (min) == True = 12 + hora
  | otherwise = -1

minutosDecorridos :: Hora -> Int
minutosDecorridos (AM hora min)
  | validaHora (hora) == True && validaMin (min) == True = hora * 60 + min
  | otherwise = -1
minutosDecorridos (PM hora min)
  | validaHora (hora) == True && validaMin (min) == True = ((12 + hora) * 60) + min
  | otherwise = -1

segundosDecorridos :: Hora -> Int
segundosDecorridos (AM hora min)
  | validaHora (hora) == True && validaMin (min) == True = (hora * 60 + min) * 60
  | otherwise = -1
segundosDecorridos (PM hora min)
  | validaHora (hora) == True && validaMin (min) == True = (((12 + hora) * 60) + min) * 60
  | otherwise = -1

--Ex8
