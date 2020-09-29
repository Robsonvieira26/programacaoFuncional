--Ex1
paridade :: Integral a => [a] -> [Bool]
paridade lista = map (even) lista

--Ex2
prefixos :: [String] -> [String]
prefixos lista = map (take 3) lista

--Ex3
saudacao :: [[Char]] -> [[Char]]
saudacao lista = map ("Oi " ++) lista

--Ex4
filtrar :: (a -> Bool) -> [a] -> [a]
filtrar _ [] = []
filtrar func (x : xs)
  | func x = x : filtrar func xs
  | otherwise = filtrar func xs

filtrarCompr :: (a -> Bool) -> [a] -> [a]
filtrarCompr func lista = [x | x <- lista, func x]

--Ex5
pares :: Integral a => [a] -> [a]
pares lista = filter (even) lista

--Ex6
solucoes :: (Ord a, Num a) => [a] -> [a]
solucoes l = filter (\x -> ((5 * x) + 6) < (x * x)) l

--Ex7
maior :: (Foldable t, Ord a) => t a -> a
maior lista = foldr1 max lista

--Ex8
menor_min10 :: (Foldable t, Ord b, Num b) => t b -> b
menor_min10 lista = foldr (min) 10 lista

--Ex9
junta_silabasPlural :: Foldable t => t [Char] -> [Char]
junta_silabasPlural lista = foldr (++) "s" lista

--Listas
lst1 :: [Integer]
lst1 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]

lst2 :: [Integer]
lst2 = [20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1]

lst3 :: [Integer]
lst3 = [11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

lst4 :: [Integer]
lst4 = [10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11]

lst5 :: [Integer]
lst5 = [11, 12, 13, 14, 15, 5, 4, 3, 2, 1, 16, 17, 18, 19, 20, 10, 9, 8, 7, 6]

lst6 :: [Integer]
lst6 = [1, 12, 3, 14, 5, 15, 4, 13, 2, 11, 6, 17, 8, 19, 20, 10, 9, 18, 7, 16]

lst7 :: [Integer]
lst7 = [1 .. 1000]

lst8 :: [Integer]
lst8 = [1000, 999 .. 1]

lst9 :: [Integer]
lst9 = lst1 ++ [0]

lst10 :: [Integer]
lst10 = [0] ++ lst3

lst11 :: [Integer]
lst11 = lst1 ++ [0] ++ lst3

lst12 :: [Integer]
lst12 = lst3 ++ [0] ++ lst1

--

--Ex10
--Bubble
myBubblesort :: Ord a => [a] -> [a]
myBubblesort [] = []
myBubblesort lista = bolhaOrd lista (length lista)

bolhaOrd :: (Num t, Ord a, Eq t) => [a] -> t -> [a]
bolhaOrd lista 0 = lista
bolhaOrd lista n = bolhaOrd (troca lista) (n -1)

troca :: Ord a => [a] -> [a]
troca [x] = [x]
troca (x : y : zs)
  | x > y = y : troca (x : zs)
  | otherwise = x : troca (y : zs)

-- Selection
mySelectionsort :: Ord a => [a] -> [a]
mySelectionsort [] = []
mySelectionsort xs = [x] ++ mySelectionsort (remove x xs)
  where
    x = minimo xs

remove :: Eq t => t -> [t] -> [t]
remove a [] = []
remove a (x : xs)
  | a == x = xs
  | otherwise = x : (remove a xs)

minimo :: Ord a => [a] -> a
minimo [] = undefined
minimo [x] = x
minimo (x : xs)
  | x <= (minimo xs) = x
  | otherwise = minimo xs

--Insertion
myInsertionsort :: (Ord a) => [a] -> [a]
myInsertionsort [] = []
myInsertionsort (x : xs) = insereOrd x (myInsertionsort xs)

insereOrd :: Ord t => t -> [t] -> [t]
insereOrd x [] = [x]
insereOrd x (y : ys)
  | x <= y = (x : y : ys)
  | otherwise = y : (insereOrd x ys)

--Quick
myQuicksort :: (Ord a) => [a] -> [a]
myQuicksort [] = []
myQuicksort (s : xs) = myQuicksort [x | x <- xs, x < s] ++ [s] ++ myQuicksort [x | x <- xs, x >= s]

--Ex11 --
--Bubble

myBubblesort2 :: (Ord a) => [a] -> ([a], Int)
myBubblesort2 [] = ([], 0)
myBubblesort2 lista = bolhaOrd2 (lista, 0) (length lista)

bolhaOrd2 :: (Ord a, Num t, Num b, Eq t) => ([a], b) -> t -> ([a], b)
bolhaOrd2 (lista, count) 0 = (lista, count)
bolhaOrd2 (lista, count) n = bolhaOrd2 (troca2 (lista, count)) (n -1)

troca2 :: (Ord a, Num b) => ([a], b) -> ([a], b)
troca2 ([x], cont) = ([x], cont)
troca2 ((x : y : zs), cont) =
  if x > y
    then add (troca2 ((x : zs), cont + 1)) y
    else add (troca2 ((y : zs), cont + 1)) x
  where
    add (lista, count) a = (a : lista, count)

-- Selection

mySelectionsort2 :: Ord a => [a] -> ([a], Int)
mySelectionsort2 lista = selectionAUX lista 0

selectionAUX :: (Ord a) => [a] -> Int -> ([a], Int)
selectionAUX [] n = ([], n)
selectionAUX (x : xs) n =
  let (least, n_num) = minimo2 (x : xs) n

      remove2 _ [] = []
      remove2 n (h : t) =
        if (n == h)
          then t
          else h : (remove2 n t)

      add (lst, n) y = (y : lst, n)
   in add (selectionAUX (remove2 least (x : xs)) n_num) least

minimo2 :: (Ord a) => [a] -> Int -> (a, Int)
minimo2 [] _ = undefined
minimo2 [x] cont = (x, cont)
minimo2 (x : y : xs) cont
  | x > y = minimo2 (y : xs) (cont + 1)
  | otherwise = minimo2 (x : xs) (cont + 1)

--Insertion

myInsertionsort2 :: (Ord a) => [a] -> ([a], Int)
myInsertionsort2 [] = ([], 0)
myInsertionsort2 [x] = ([x], 0)
myInsertionsort2 (h : t) =
  let (sorted_tail, n) = myInsertionsort2 t

      (lst, n1) = insereOrd2 h sorted_tail n
   in (lst, n1)

insereOrd2 :: (Ord a) => a -> [a] -> Int -> ([a], Int)
insereOrd2 x [] n = ([x], n)
insereOrd2 x (h : t) n =
  if (x <= h)
    then ((x : h : t), n + 1)
    else add (insereOrd2 x t (n + 1)) h
  where
    add (list, n) y = (y : list, n)

--Quick
quickAux :: [a] -> Int -> (a -> Bool) -> ([a], Int)
quickAux [] n _ = ([], n)
quickAux (x : xs) n cond =
  if (cond x)
    then add (quickAux xs (n + 1) cond) x
    else quickAux xs (n + 1) cond
  where
    add (list, n) y = (y : list, n)

myQuicksort2 :: (Ord a) => [a] -> ([a], Int)
myQuicksort2 [] = ([], 0)
myQuicksort2 (piv : xs) =
  let (left, n_L) = quickAux xs 0 (<= piv)
      (right, n_R) = quickAux xs 0 (> piv)
      (sorted_L, n1_L) = myQuicksort2 left
      (sorted_R, n1_R) = myQuicksort2 right
   in (sorted_L ++ [piv] ++ sorted_R, n_L + n_R + n1_L + n1_R)

--Ex12
--Bubble
myBubblesort3 :: (Ord a) => [a] -> ([a], Int)
myBubblesort3 [] = ([], 0)
myBubblesort3 lista = bolhaOrd3 (lista, 0) (length lista)

bolhaOrd3 :: (Ord a, Num t, Num b, Eq t) => ([a], b) -> t -> ([a], b)
bolhaOrd3 (lista, count) 0 = (lista, count)
bolhaOrd3 (lista, count) n = bolhaOrd3 (troca3 (lista, count)) (n -1)

troca3 :: (Ord a, Num b) => ([a], b) -> ([a], b)
troca3 ([x], cont) = ([x], cont)
troca3 ((x : y : zs), cont) =
  if x > y
    then add (troca3 ((y : zs), cont + 1)) x
    else add (troca3 ((x : zs), cont + 1)) y
  where
    add (lista, count) a = (a : lista, count)

-- Selection

mySelectionsort3 :: Ord a => [a] -> ([a], Int)
mySelectionsort3 lista = selectionAUX2 lista 0

selectionAUX2 :: (Ord a) => [a] -> Int -> ([a], Int)
selectionAUX2 [] n = ([], n)
selectionAUX2 (x : xs) n =
  let (least, n_num) = minimo3 (x : xs) n

      remove3 _ [] = []
      remove3 n (h : t) =
        if (n == h)
          then t
          else h : (remove3 n t)

      add (lst, n) y = (y : lst, n)
   in add (selectionAUX2 (remove3 least (x : xs)) n_num) least

minimo3 :: (Ord a) => [a] -> Int -> (a, Int)
minimo3 [] _ = undefined
minimo3 [x] cont = (x, cont)
minimo3 (x : y : xs) cont
  | x > y = minimo3 (x : xs) (cont + 1)
  | otherwise = minimo3 (y : xs) (cont + 1)

--Insertion

myInsertionsort3 :: (Ord a) => [a] -> ([a], Int)
myInsertionsort3 [] = ([], 0)
myInsertionsort3 [x] = ([x], 0)
myInsertionsort3 (h : t) =
  let (sorted_tail, n) = myInsertionsort3 t

      (lst, n1) = insereOrd3 h sorted_tail n
   in (lst, n1)

insereOrd3 :: (Ord a) => a -> [a] -> Int -> ([a], Int)
insereOrd3 x [] n = ([x], n)
insereOrd3 x (h : t) n =
  if (x >= h)
    then ((x : h : t), n + 1)
    else add (insereOrd3 x t (n + 1)) h
  where
    add (list, n) y = (y : list, n)

--Quick
quickAux2 :: [a] -> Int -> (a -> Bool) -> ([a], Int)
quickAux2 [] n _ = ([], n)
quickAux2 (x : xs) n cond =
  if (cond x)
    then quickAux2 xs (n + 1) cond
    else add (quickAux2 xs (n + 1) cond) x
  where
    add (list, n) y = (y : list, n)

myQuicksort3 :: (Ord a) => [a] -> ([a], Int)
myQuicksort3 [] = ([], 0)
myQuicksort3 (piv : xs) =
  let (left, n_L) = quickAux2 xs 0 (<= piv)
      (right, n_R) = quickAux2 xs 0 (> piv)
      (sorted_L, n1_L) = myQuicksort3 left
      (sorted_R, n1_R) = myQuicksort3 right
   in (sorted_L ++ [piv] ++ sorted_R, n_L + n_R + n1_L + n1_R)
