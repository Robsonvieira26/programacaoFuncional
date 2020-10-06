--Alunos:
--Loredana Devico e Robson Vieira
--Listas
l1 :: [Integer]
l1 = [1 .. 1000]

l2 :: [Integer]
l2 = [1000, 999 .. 1]

l3 :: [Integer]
l3 = l1 ++ [0]

l4 :: [Integer]
l4 = [0] ++ l2

l5 :: [Integer]
l5 = l1 ++ [0] ++ l2

l6 :: [Integer]
l6 = l2 ++ [0] ++ l1

l7 :: [Integer]
l7 = l2 ++ [0] ++ l2

x1 :: [Integer]
x1 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]

x2 :: [Integer]
x2 = [20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1]

x3 :: [Integer]
x3 = [11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

x4 :: [Integer]
x4 = [10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11]

x5 :: [Integer]
x5 = [11, 12, 13, 14, 15, 5, 4, 3, 2, 1, 16, 17, 18, 19, 20, 10, 9, 8, 7, 6]

x6 :: [Integer]
x6 = [1, 12, 3, 14, 5, 15, 4, 13, 2, 11, 6, 17, 8, 19, 20, 10, 9, 18, 7, 16]

x7 :: [Integer]
x7 = [20, 8, 2, 11, 13, 3, 7, 18, 14, 4, 16, 10, 15, 1, 9, 17, 19, 12, 5, 6]

--Ex1

-- Selection Sort
selectionSort :: (Ord a) => [a] -> [a]
selectionSort [] = []
selectionSort [x] = [x]
selectionSort (x : xs) =
  let least = foldr1 (min) (x : xs)
      remove _ [] = []
      remove n (h : t) =
        if n == h
          then t
          else h : (remove n t)
   in [least] ++ selectionSort (remove least (x : xs))

-- Insertion Sort
insertionSort :: (Ord a) => [a] -> [a]
insertionSort l = foldr (insereOrd) [] l
  where
    insereOrd x [] = [x]
    insereOrd x (h : t) =
      if x <= h
        then (x : h : t)
        else h : (insereOrd x t)

-- Quick Sort
quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (piv : xs) = quickSort (filter (< piv) xs) ++ [piv] ++ quickSort (filter (>= piv) xs)

--Ex2
-- Variação 1
troca :: (Ord a) => ([a], Int) -> ([a], Int)
troca ([x], flag) = ([x], flag)
troca ((x : y : xs), flag) =
  if x > y
    then add (troca ((x : xs), 1)) y
    else add (troca ((y : xs), flag)) x
  where
    add (l, f) e = (e : l, f)

bubbleAux :: (Ord a) => ([a], Int) -> Int -> ([a], Int)
bubbleAux (l, flag) 0 = (l, flag)
bubbleAux (l, flag) n
  | flag == 0 = (l, flag)
  | otherwise = bubbleAux (troca (l, 0)) (n -1)

bubbleSort1 :: (Ord a) => [a] -> [a]
bubbleSort1 [] = []
bubbleSort1 list = fst (bubbleAux (list, -1) (length list))

-- Variação 2
bubbleSort2 :: (Ord a) => [a] -> [a]
bubbleSort2 [] = []
bubbleSort2 lst =
  let troca [x] = [x]
      troca (x : y : xs) =
        if x > y
          then y : troca (x : xs)
          else x : troca (y : xs)

      split lst = (take (length lst - 1) lst, drop (length lst - 1) lst)

      bubble [x] = [x]
      bubble l = (bubble aTrocar) ++ ultimoElm
        where
          listaMod = troca l
          (aTrocar, ultimoElm) = split listaMod
   in bubble lst

-- Variação 3
bubbleSort3 :: (Ord a) => [a] -> [a]
bubbleSort3 [] = []
bubbleSort3 l =
  let add (l, f) y = (y : l, f)
      split lst = (take (length lst - 1) lst, drop (length lst - 1) lst)

      troca ([x], flag) = ([x], flag)
      troca ((x : y : xs), flag) =
        if x > y
          then add (troca ((x : xs), 1)) y
          else add (troca ((y : xs), flag)) x

      bubble ([x], flag) = ([x], flag)
      bubble (lst, flag)
        | n_flag == 0 = (lst, flag)
        | otherwise = (fst (bubble (paraTrocar, 0)) ++ ultElem, 0)
        where
          (listaTrocada, n_flag) = troca (lst, flag)
          (paraTrocar, ultElem) = split listaTrocada
   in fst (bubble (l, -1))

-- COM CONTAGEM

-- Variação 1
bubbleSort1Cont :: (Ord a) => [a] -> ([a], Int)
bubbleSort1Cont [] = ([], 0)
bubbleSort1Cont list = format (bubleAuxCont (list, -1, 0) (length list))
  where
    format (l, _, c) = (l, c)

trocaCont :: (Ord a) => ([a], Int, Int) -> ([a], Int, Int)
trocaCont ([x], flag, n) = ([x], flag, n)
trocaCont ((x : y : xs), flag, n) =
  if x > y
    then add (trocaCont ((x : xs), 1, n + 1)) y
    else add (trocaCont ((y : xs), flag, n + 1)) x
  where
    add (l, f, c) e = (e : l, f, c)

bubleAuxCont :: (Ord a) => ([a], Int, Int) -> Int -> ([a], Int, Int)
bubleAuxCont (l, flag, c) 0 = (l, flag, c)
bubleAuxCont (l, flag, c) n
  | flag == 0 = (l, flag, c)
  | otherwise = bubleAuxCont (trocaCont (l, 0, c)) (n -1)

-- Variação 2
bubbleSort2Cont :: (Ord a) => [a] -> ([a], Int)
bubbleSort2Cont [] = ([], 0)
bubbleSort2Cont lst =
  let add (l, c) e = (e : l, c)

      troca ([x], c) = ([x], c)
      troca ((x : y : xs), c) =
        if x > y
          then add (troca (x : xs, c + 1)) y
          else add (troca (y : xs, c + 1)) x

      split lst = (take (length lst - 1) lst, drop (length lst - 1) lst)

      bubble :: (Ord a) => ([a], Int) -> ([a], Int)
      bubble ([x], c) = ([x], c)
      bubble (l, c) = (proxEtapa ++ ultElem, rec_c)
        where
          (listaTrocada, c1) = (troca (l, c))
          (paraTrocar, ultElem) = split listaTrocada
          (proxEtapa, rec_c) = bubble (paraTrocar, c1)
   in bubble (lst, 0)

-- Variação 3
bubbleSort3Cont :: (Ord a) => [a] -> ([a], Int)
bubbleSort3Cont [] = ([], 0)
bubbleSort3Cont l =
  let add (l, f, c) y = (y : l, f, c)
      split lst = (take (length lst - 1) lst, drop (length lst - 1) lst)
      format (l, _, c) = (l, c)

      troca ([x], flag, c) = ([x], flag, c)
      troca ((x : y : xs), flag, c) =
        if x > y
          then add (troca ((x : xs), 1, c + 1)) y
          else add (troca ((y : xs), flag, c + 1)) x

      bubble ([x], flag, c) = ([x], flag, c)
      bubble (lst, flag, c)
        | n_flag == 0 = (lst, flag, c)
        | otherwise = (proxEtapa ++ ultElem, 0, rec_c)
        where
          (listaTrocada, n_flag, c1) = troca (lst, flag, c)
          (paraTrocar, ultElem) = split listaTrocada
          (proxEtapa, _, rec_c) = bubble (paraTrocar, 0, c1)
   in format (bubble (l, -1, 0))

--Resposta no arquivo Respostas.MD
--Ex3
-- Variação 1
selectionSortEx3 :: (Ord a) => [a] -> [a]
selectionSortEx3 [] = []
selectionSortEx3 [x] = [x]
selectionSortEx3 (x : xs) =
  let least = foldr1 (min) (x : xs)

      remove _ [] = []
      remove n (h : t) =
        if n == h
          then t
          else h : (remove n t)
   in least : selectionSortEx3 (remove least (x : xs))

-- Variação 2
removeMenor :: (Ord a) => (a, [a]) -> (a, [a])
removeMenor (m, [x]) =
  if x < m
    then (x, [m])
    else (m, [x])
removeMenor (menor, (x : xs))
  | x < menor = add menor (removeMenor (x, xs))
  | otherwise = add x (removeMenor (menor, xs))
  where
    add a (n, l) = (n, a : l)

selectionSort2 :: (Ord a) => [a] -> [a]
selectionSort2 [] = []
selectionSort2 [x] = [x]
selectionSort2 lst =
  let (least, novoUlt) = removeMenor (head lst, tail lst)
   in least : (selectionSort2 novoUlt)

-- COM CONTAGEM
removeMenorCont :: (Ord a) => (a, [a], Int) -> (a, [a], Int)
removeMenorCont (m, [x], c) =
  if x < m
    then (x, [m], c + 1)
    else (m, [x], c + 1)
removeMenorCont (menor, (x : xs), c1)
  | x < menor = add menor (removeMenorCont (x, xs, c1 + 1))
  | otherwise = add x (removeMenorCont (menor, xs, c1 + 1))
  where
    add a (n, l, c) = (n, a : l, c)

selectionSort2Cont :: (Ord a) => [a] -> ([a], Int)
selectionSort2Cont [] = ([], 0)
selectionSort2Cont [x] = ([x], 0)
selectionSort2Cont (x : xs) =
  let (least, novoUlt, cont) = removeMenorCont (x, xs, 0)
      (proxEtapa, nCont) = selectionSort2Cont novoUlt
   in (least : proxEtapa, cont + nCont)

--Resposta no arquivo Respostas.MD

--Ex4
-- Variação 1
divide :: (Ord a) => a -> [a] -> ([a], [a])
divide _ [] = ([], [])
divide x [e] =
  if e < x
    then ([e], [])
    else ([], [e])
divide x (e : es)
  | e < x = addEsq e (divide x es)
  | otherwise = addDir e (divide x es)
  where
    addEsq a (l, r) = (a : l, r)
    addDir a (l, r) = (l, a : r)

quickSortEx4 :: (Ord a) => [a] -> [a]
quickSortEx4 [] = []
quickSortEx4 (piv : xs) =
  let (left, right) = divide piv xs
   in (quickSortEx4 left) ++ [piv] ++ (quickSortEx4 right)

-- Variação 2
quickSort2 :: (Ord a) => [a] -> [a]
quickSort2 [] = []
quickSort2 lst =
  let firstThree = take 3 lst
      piv =
        if length (firstThree) < 3
          then firstThree !! 0 --acesso ao firstThree no index 0
          else foldr1 (min) (firstThree)

      deletaPrimOcorrencia _ [] = []
      deletaPrimOcorrencia x (y : ys)
        | x == y = ys
        | otherwise = y : deletaPrimOcorrencia x ys
      (left, right) = divide piv (deletaPrimOcorrencia piv lst)
   in (quickSort2 left) ++ [piv] ++ (quickSort2 right)

-- COM CONTAGEM
divideCont :: (Ord a) => a -> [a] -> Int -> ([a], [a], Int)
divideCont _ [] n = ([], [], n)
divideCont x [e] n =
  if e < x
    then ([e], [], n + 1)
    else ([], [e], n + 1)
divideCont x (e : es) n
  | e < x = addEsq e (divideCont x es (n + 1))
  | otherwise = addDir e (divideCont x es (n + 1))
  where
    addEsq a (l, r, c) = (a : l, r, c)
    addDir a (l, r, c) = (l, a : r, c)

quickSortCont :: (Ord a) => [a] -> ([a], Int)
quickSortCont [] = ([], 0)
quickSortCont (piv : xs) =
  let (left, right, n) = divideCont piv xs 0
      (sortedL, n_L) = quickSortCont left
      (sortedR, n_R) = quickSortCont right
   in (sortedL ++ [piv] ++ sortedR, n + n_L + n_R)

quickSortCount2 :: (Ord a) => [a] -> ([a], Int)
quickSortCount2 [] = ([], 0)
quickSortCount2 lst =
  let piv = foldr1 (min) (take 3 lst)
      deleteFrstOc :: (Ord a) => a -> [a] -> Int -> ([a], Int)
      deleteFrstOc _ [] n = ([], n)
      deleteFrstOc x (y : ys) n
        | x == y = (ys, n + 1)
        | otherwise = add y (deleteFrstOc x ys (n + 1))
        where
          add e (l, c) = (e : l, c)
      (novoUlt, checks) = deleteFrstOc piv lst 0
      (left, right, n1) = divideCont piv novoUlt 0
      (sortedL, n_L) = quickSortCount2 left
      (sortedR, n_R) = quickSortCount2 right
   in (sortedL ++ [piv] ++ sortedR, n1 + n_L + n_R + checks + 3) -- Comps. atuais + comps recursivas + comps do deletaPrimOcorrencia + 3 comps. do foldr1

--Resposta no arquivo Respostas.MD

-- Ex5

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] [] = []
merge l1 [] = l1
merge [] l2 = l2
merge (a : as) (b : bs)
  | a > b = b : (merge (a : as) bs)
  | otherwise = a : (merge as (b : bs))

mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort lst =
  let left = mergeSort (take ((length lst) `div` 2) lst)
      right = mergeSort (drop ((length lst) `div` 2) lst)
   in merge left right

-- Bucket Sort
sortIntoBuckets :: Int -> Int -> Int -> Int -> [[Int]] -> [[Int]]
sortIntoBuckets num k m n [bucket] =
  if ((num * k) `div` m) <= n
    then [num : bucket]
    else [bucket]
sortIntoBuckets num k m n (bucket : buckets)
  | ((num * k) `div` m) <= n = (num : bucket) : buckets
  | otherwise = bucket : (sortIntoBuckets num k m (n + 1) buckets)

bucketSort :: [Int] -> [Int]
bucketSort [] = []
bucketSort [x] = [x]
bucketSort l1 =
  let k = length l1
      m = foldr1 (max) l1
      buckets = [[] | _ <- [1 .. k]]
      newBuckets = foldr (\x -> sortIntoBuckets x k m 1) buckets l1
      sortedBuckets = map (mergeSort) newBuckets
      finalList = foldr1 (++) sortedBuckets
   in finalList

--Resposta no arquivo Respostas.MD
