--Ex1
paridade :: Integral a => [a] -> [Bool]
paridade lista = map (even) lista

--Ex2
prefixos :: [[a]] -> [[a]]
prefixos lista = map (take 3) lista

--Ex3
saudacao :: [[Char]] -> [[Char]]
saudacao lista = map ("Oi " ++) lista

--Ex4
filtrar :: (a -> Bool) -> [a] -> [a]
filtrar _ [] = []
filtrar p (x : xs)
  | p x = x : filtrar p xs
  | otherwise = filtrar p xs

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

myQuicksort :: (Ord a) => [a] -> [a]
myQuicksort [] = []
myQuicksort (s : xs) = myQuicksort [x | x <- xs, x < s] ++ [s] ++ myQuicksort [x | x <- xs, x >= s]

--Ex11--

--Ex12
