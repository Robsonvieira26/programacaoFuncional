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

--Todo:  filtrarCompr

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

--Ex10
--todo:myBubblesort
--todo:mySelectionsort
--todo:myInsertionsort
--todo:myQuicksort
--Ex11
--Ex12
