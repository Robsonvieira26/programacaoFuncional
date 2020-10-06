--Alunos
--Loredana Devico e Robson Roberto
data Exp a
  = Val a -- um numero
  | Add (Exp a) (Exp a) -- soma de duas expressoes
  | Sub (Exp a) (Exp a) --subtração
  | Mult (Exp a) (Exp a)
  | Pot (Exp a) (Exp a)
  deriving (Show)

avalia :: Floating a => Exp a -> a
avalia (Val x) = x
avalia (Add exp1 exp2) = (avalia exp1) + (avalia exp2)
avalia (Sub exp1 exp2) = (avalia exp1) - (avalia exp2)
avalia (Mult exp1 exp2) = (avalia exp1) * (avalia exp2)
avalia (Pot exp1 exp2) = (avalia exp1) ** (avalia exp2)

--B)
expre1 :: Num a => Exp a
expre1 = (Mult (Add (Val 3) (Val 12)) (Pot (Sub (Val 15) (Val 5)) (Mult (Val 1) (Val 3))))

expre2 :: Num a => Exp a
expre2 = (Sub (Val 0) (Mult (Add (Add (Val 6) (Val 8)) (Sub (Val 1) (Val 5))) (Add (Val 2) (Pot (Val 6) (Val 2)))))

--Ex7

data Hora
  = PM Int Int
  | AM Int Int
  deriving (Eq, Show, Ord)

validaHora :: Int -> Bool
validaHora h
  | h > 0 && h <= 11 = True
  | otherwise = False

validaMin :: Int -> Bool
validaMin m
  | m >= 0 && m <= 59 = True
  | otherwise = False

horasDecorridas :: Hora -> Int
horasDecorridas (AM hora min)
  | validaHora (hora) == True && validaMin (min) == True = hora
  | otherwise = undefined
horasDecorridas (PM hora min)
  | validaHora (hora) == True && validaMin (min) == True = 12 + hora
  | otherwise = undefined

minutosDecorridos :: Hora -> Int
minutosDecorridos (AM hora min)
  | validaHora (hora) == True && validaMin (min) == True = hora * 60 + min
  | otherwise = undefined
minutosDecorridos (PM hora min)
  | validaHora (hora) == True && validaMin (min) == True = ((12 + hora) * 60) + min
  | otherwise = undefined

segundosDecorridos :: Hora -> Int
segundosDecorridos (AM hora min)
  | validaHora (hora) == True && validaMin (min) == True = (hora * 60 + min) * 60
  | otherwise = undefined
segundosDecorridos (PM hora min)
  | validaHora (hora) == True && validaMin (min) == True = (((12 + hora) * 60) + min) * 60
  | otherwise = undefined

--Ex8

type Data = (Int, Int, Int)

precede :: Data -> Data -> Bool
precede (d1, m1, y1) (d2, m2, y2)
  | y1 > y2 = False
  | y1 == y2 && m1 > m2 = False
  | y1 == y2 && m1 == m2 && d1 > d2 = False
  | otherwise = True

data Contato = Nome String | Fone String
  deriving (Eq, Show)

data Mensagem = Msg Contato String Data Hora String
  deriving (Show)

--A)
msgRecebidas :: [Mensagem]
msgRecebidas =
  [ (Msg (Nome "Fulano") "Mensagem 1" (13, 08, 20) (AM 10 30) "WhatsApp"),
    (Msg (Fone "123456") "Mensagem 2" (13, 08, 20) (AM 10 31) "WhatsApp"),
    (Msg (Nome "Fulana") "Mensagem 3" (13, 08, 20) (AM 10 32) "LinkedIn"),
    (Msg (Nome "Fulana") "Mensagem 4" (13, 08, 20) (AM 10 33) "WhatsApp"),
    (Msg (Nome "Fulano") "Mensagem 5" (13, 08, 20) (AM 10 37) "Facebook"),
    (Msg (Nome "Fulana") "Mensagem 6" (13, 08, 20) (AM 11 30) "Facebook"),
    (Msg (Nome "Fulana") "Mensagem 7" (13, 08, 20) (AM 11 35) "WhatsApp"),
    (Msg (Fone "123456") "Mensagem 8" (13, 08, 20) (AM 11 37) "LinkedIn"),
    (Msg (Nome "Fulano") "Mensagem 9" (13, 08, 20) (AM 11 39) "Facebook"),
    (Msg (Nome "Fulano") "Mensagem 11" (13, 08, 20) (AM 11 42) "Facebook"),
    (Msg (Nome "Fulana") "Mensagem 11" (13, 08, 20) (AM 11 42) "WhatsApp"),
    (Msg (Fone "123456") "Mensagem 12" (13, 08, 20) (AM 11 53) "Facebook"),
    (Msg (Nome "Fulano") "Mensagem 13" (13, 08, 20) (AM 11 53) "WhatsApp"),
    (Msg (Nome "Fulana") "Mensagem 14" (13, 08, 20) (AM 11 54) "Facebook"),
    (Msg (Nome "Fulana") "Mensagem 15" (13, 08, 20) (AM 11 54) "WhatsApp"),
    -- =======(Nome ========)=======================================
    (Msg (Nome "Fulano") "Mensagem 16" (14, 08, 20) (PM 3 25) "WhatsApp"),
    (Msg (Nome "Fulano") "Mensagem 17" (14, 08, 20) (PM 3 25) "LinkedIn"),
    (Msg (Nome "Fulano") "Mensagem 18" (14, 08, 20) (PM 3 24) "Facebook"),
    (Msg (Nome "Fulana") "Mensagem 19" (14, 08, 20) (PM 3 27) "Facebook"),
    (Msg (Nome "Fulano") "Mensagem 20" (14, 08, 20) (PM 3 30) "WhatsApp"),
    (Msg (Nome "Fulano") "Mensagem 21" (14, 08, 20) (PM 3 33) "Facebook"),
    (Msg (Nome "Fulana") "Mensagem 22" (14, 08, 20) (PM 3 49) "WhatsApp"),
    (Msg (Fone "123456") "Mensagem 23" (14, 08, 20) (PM 4 50) "LinkedIn"),
    (Msg (Nome "Fulano") "Mensagem 24" (14, 08, 20) (PM 4 57) "WhatsApp"),
    (Msg (Nome "Fulana") "Mensagem 25" (14, 08, 20) (PM 4 30) "Facebook"),
    (Msg (Nome "Fulano") "Mensagem 26" (14, 08, 20) (PM 4 30) "WhatsApp"),
    (Msg (Fone "123456") "Mensagem 27" (14, 08, 20) (PM 4 30) "LinkedIn"),
    (Msg (Nome "Fulana") "Mensagem 28" (14, 08, 20) (PM 4 30) "Facebook"),
    (Msg (Fone "123456") "Mensagem 29" (14, 08, 20) (PM 4 30) "WhatsApp"),
    (Msg (Nome "Fulano") "Mensagem 30" (14, 08, 20) (PM 4 30) "LinkedIn")
  ]

--B)
ordenarPorContato :: [Mensagem] -> [Mensagem]
ordenarPorContato [] = []
ordenarPorContato list = bubble list (length list)

bubble :: [Mensagem] -> Int -> [Mensagem]
bubble list 0 = list
bubble list n = bubble (troca list) (n -1)

troca :: [Mensagem] -> [Mensagem]
troca [x] = [x]
troca (msg1 : msg2 : xs)
  | comparacao msg1 msg2 = msg2 : troca (msg1 : xs)
  | otherwise = msg1 : troca (msg2 : xs)
  where
    comparacao (Msg (Nome _) _ _ _ _) (Msg (Fone _) _ _ _ _) = True -- Ocorre troca, fone vem primeiro
    comparacao (Msg (Fone _) _ _ _ _) (Msg (Nome _) _ _ _ _) = False -- Não ocorre troca
    comparacao (Msg (Nome nome1) _ _ _ _) (Msg (Nome nome2) _ _ _ _) = nome1 > nome2
    comparacao (Msg (Fone nome1) _ _ _ _) (Msg (Fone nome2) _ _ _ _) = nome1 > nome2

--C)
msgProcede :: Mensagem -> Mensagem -> Bool
msgProcede (Msg _ _ data1 hora1 _) (Msg _ _ data2 hora2 _)
  | data1 == data2 = (minutosDecorridos hora1) < (minutosDecorridos hora2)
  | otherwise = precede data1 data2

ordenaDataHora :: [Mensagem] -> [Mensagem]
ordenaDataHora [] = []
ordenaDataHora (piv : xs) =
  (ordenaDataHora [x | x <- xs, (msgProcede x piv) == False])
    ++ [piv]
    ++ (ordenaDataHora [x | x <- xs, (msgProcede x piv) == True])

--D)
ultimasMsgs :: Contato -> [Mensagem] -> [Mensagem]
ultimasMsgs contact msgs = take 2 [(Msg c m d h a) | (Msg c m d h a) <- msgOrd, c == contact]
  where
    msgOrd = ordenaDataHora msgs

--Ex9)
data ArvBinInt
  = Nulo
  | No Int ArvBinInt ArvBinInt
  deriving (Show)

arvDados :: ArvBinInt
arvDados =
  No
    4
    (No 2 Nulo Nulo)
    ( No
        10
        (No 5 Nulo Nulo)
        (No 15 Nulo Nulo)
    )

{-- ArvDados é essa
         4
      2     10
          5   15
--}
--A)
internos :: ArvBinInt -> [Int]
internos Nulo = []
internos (No _ Nulo Nulo) = []
internos (No n esq dir) = [n] ++ internos esq ++ internos dir

--B)
somaNos :: ArvBinInt -> Int
somaNos Nulo = 0
somaNos (No n Nulo Nulo) = n --no folha
somaNos (No n esq dir) = n + somaNos esq + somaNos dir --soma n com a soma dos filhos da dir e da esq
--C)

pertenceArv :: Int -> ArvBinInt -> Bool
pertenceArv _ Nulo = False
pertenceArv x (No v esq dir) =
  x == v --compara o valor com o no
    || if x < v --escolhe pra qual lado da arvore vai
      then (pertenceArv x esq)
      else (pertenceArv x dir)

--Ex 10
data ArvBinEA a
  = Vazia
  | Folha a
  | NoEA (Char, ArvBinEA a, ArvBinEA a)
  deriving (Show)

arvEA :: ArvBinEA Float
arvEA = NoEA ('+', NoEA ('*', Folha 10, Folha 5), Folha 7)

{--
       +
     *   7
   10 5
--}

calculaArv :: (Fractional a, Integral a) => ArvBinEA a -> a
calculaArv Vazia = 0
calculaArv (Folha valor) = valor
calculaArv (NoEA (operacao, esq, dir))
  | (operacao == '+') = (calculaArv esq) + (calculaArv dir)
  | (operacao == '*') = (calculaArv esq) * (calculaArv dir)
  | (operacao == '/') = (calculaArv esq) / (calculaArv dir)
  | (operacao == '^') = (calculaArv esq) ^ (calculaArv dir)
  | (operacao == '-') = (calculaArv esq) - (calculaArv dir)
