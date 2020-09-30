--Alunos:
--Loredana Devico e Robson Roberto
data Exp a
  = Val a -- um numero
  | Add (Exp a) (Exp a) -- soma de duas expressoes
  | Sub (Exp a) (Exp a) --subtração
  | Mult (Exp a) (Exp a)
  | Pot (Exp a) (Exp a)

avalia :: Floating a => Exp a -> a
avalia (Val x) = x
avalia (Add exp1 exp2) = (avalia exp1) + (avalia exp2)
avalia (Sub exp1 exp2) = (avalia exp1) - (avalia exp2)
avalia (Mult exp1 exp2) = (avalia exp1) * (avalia exp2)
avalia (Pot exp1 exp2) = (avalia exp1) ** (avalia exp2)

expre1 :: Exp Integer
expre1 = (Mult (Add (Val 3) (Val 12)) (Pot (Sub (Val 15) (Val 5)) (Mult (Val 1) (Val 3))))

expre2 :: Exp Integer
expre2 = (Sub (Val 0) (Mult (Add (Add (Val 6) (Val 8)) (Sub (Val 1) (Val 5))) (Add (Val 2) (Pot (Val 6) (Val 2)))))

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
data Contato
  = Nome String
  | Fone String

type Texto = String

type Data = (Int, Int, Int)

data Mensagem
  = WhatsApp Contato Texto Hora Data
  | LinkedIn Contato Texto Hora Data
  | Facebook Contato Texto Hora Data

--A) lista de mensagens
msgRecebidas :: [Mensagem]
msgRecebidas =
  [ (WhatsApp (Nome "Fulano") "Mensagem 1" (AM 10 30) (13, 08, 20)),
    (LinkedIn (Fone "123456") "Mensagem 2" (AM 10 31) (13, 08, 20)),
    (Facebook (Nome "Fulana") "Mensagem 3" (AM 10 32) (13, 08, 20)),
    (WhatsApp (Nome "Fulana") "Mensagem 4" (AM 10 33) (13, 08, 20)),
    (WhatsApp (Nome "Fulano") "Mensagem 5" (AM 10 37) (13, 08, 20)),
    (Facebook (Nome "Fulana") "Mensagem 6" (AM 11 30) (13, 08, 20)),
    (WhatsApp (Nome "Fulana") "Mensagem 7" (AM 11 35) (13, 08, 20)),
    (Facebook (Fone "123456") "Mensagem 8" (AM 11 37) (13, 08, 20)),
    (LinkedIn (Nome "Fulano") "Mensagem 9" (AM 11 39) (13, 08, 20)),
    (WhatsApp (Nome "Fulano") "Mensagem 11" (AM 11 42) (13, 08, 20)),
    (LinkedIn (Nome "Fulana") "Mensagem 11" (AM 11 42) (13, 08, 20)),
    (Facebook (Fone "123456") "Mensagem 12" (AM 11 53) (13, 08, 20)),
    (WhatsApp (Nome "Fulano") "Mensagem 13" (AM 11 53) (13, 08, 20)),
    (WhatsApp (Nome "Fulana") "Mensagem 14" (AM 11 54) (13, 08, 20)),
    (LinkedIn (Nome "Fulana") "Mensagem 15" (AM 11 54) (13, 08, 20)),
    -- ======================================================
    (Facebook (Nome "Fulano") "Mensagem 16" (PM 3 25) (14, 08, 20)),
    (LinkedIn (Fone "Fulano") "Mensagem 17" (PM 3 25) (14, 08, 20)),
    (WhatsApp (Nome "Fulano") "Mensagem 18" (PM 3 24) (14, 08, 20)),
    (LinkedIn (Nome "Fulana") "Mensagem 19" (PM 3 27) (14, 08, 20)),
    (LinkedIn (Nome "Fulano") "Mensagem 20" (PM 3 30) (14, 08, 20)),
    (WhatsApp (Nome "Fulano") "Mensagem 21" (PM 3 33) (14, 08, 20)),
    (Facebook (Nome "Fulana") "Mensagem 22" (PM 3 49) (14, 08, 20)),
    (WhatsApp (Fone "123456") "Mensagem 23" (PM 4 50) (14, 08, 20)),
    (WhatsApp (Nome "Fulano") "Mensagem 24" (PM 4 57) (14, 08, 20)),
    (LinkedIn (Nome "Fulana") "Mensagem 25" (PM 4 30) (14, 08, 20)),
    (WhatsApp (Nome "Fulano") "Mensagem 26" (PM 4 30) (14, 08, 20)),
    (Facebook (Fone "123456") "Mensagem 27" (PM 4 30) (14, 08, 20)),
    (LinkedIn (Nome "Fulana") "Mensagem 28" (PM 4 30) (14, 08, 20)),
    (LinkedIn (Fone "123456") "Mensagem 29" (PM 4 30) (14, 08, 20)),
    (Facebook (Nome "Fulano") "Mensagem 30" (PM 4 30) (14, 08, 20))
  ]

--B)
--Ordenar com bubble pelo contato
myBubblesort [] = []
myBubblesort lista = bolhaOrd lista (length lista)

bolhaOrd lista 0 = lista
bolhaOrd lista n = bolhaOrd (troca lista) (n -1)

troca [x] = [x]
troca (x : y : zs)
  | x > y = y : troca (x : zs)
  | otherwise = x : troca (y : zs)

--C)
--Ordenar com Quick pela data e hora
--D)ultimas duas msgs de um contato x (usar a funçao de cima)

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
internos (No n Nulo Nulo) = []
internos (No n esq dir) = [n] ++ internos esq ++ internos dir

--B)
somaNos :: ArvBinInt -> Int
somaNos Nulo = 0
somaNos (No n Nulo Nulo) = n --no folha
somaNos (No n esq dir) = n + somaNos esq + somaNos dir --soma n com a soma dos filhos da dir e da esq
--C)

pertenceArv :: Int -> ArvBinInt -> Bool
pertenceArv x Nulo = False
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

-- inOrder = 10*5+7

inOrder :: Show a => ArvBinEA a -> [Char]
inOrder Vazia = []
inOrder (Folha valor) = show valor
inOrder (NoEA (operacao, esq, dir)) = inOrder esq ++ [operacao] ++ inOrder dir

{--
10
*
5
+
7
--}
