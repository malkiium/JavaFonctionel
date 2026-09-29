traiterPairs [] = []
traiterPairs (x:xs)
    | even x = x : traiterPairs xs
    | otherwise = 0 : traiterPairs xs

checkPair x
    | even x = x
    | otherwise = 0

traiterPairsMap li = map checkPair li

extrairePairs [] = []
extrairePairs (x:xs)
    | even x = x : extrairePairs xs
    | otherwise = extrairePairs xs

extrairePairsFilter li = filter even li

doublerPairs [] = []
doublerPairs (x:xs)
    | even x = (x*2) : doublerPairs xs
    | otherwise = doublerPairs xs

doublerPairsMap li = map (*2) (filter even li)

sommePairs [] = 0
sommePairs (x:xs)
    | even x = x + sommePairs xs
    | otherwise = sommePairs xs

ajouteSiPair acc x
    | even x = acc + x
    | otherwise = acc

sommePairsFold li = foldl ajouteSiPair 0 li

sommePairsFoldFilter li = foldl (+) 0 (filter even li)

inverser [] = []
inverser (x:xs) = inverser xs ++ [x]

clone [] = []
clone (x:xs) = [x,x] : clone xs

cloneMap li = map (\x -> [x,x]) li

clone2 n [] = []
clone2 n (x:xs) = replicate n x : clone2 n xs

clone2Map n li = map (\x -> replicate n x) li

premieresLettres li = map head (filter (\x -> length x > 2) li)

dupliquer li = map (\x -> replicate x 'x') li


puiss e 0 = 1
puiss e n = e * puiss e (n-1)

nbchiffres e
    | abs e < 10 = 1
    | otherwise = 1 + nbchiffres (abs e `div` 10)


monMap f [] = []
monMap f (x:xs) = f x : monMap f xs

monFilter f [] = []
monFilter f (x:xs)
    | f x = x : monFilter f xs
    | otherwise = monFilter f xs

monFoldl f acc [] = acc
monFoldl f acc (x:xs) = monFoldl f (f acc x) xs

monLast [x] = x
monLast (x:xs) = monLast xs

monInit [x] = []
monInit (x:xs) = x : monInit xs

monTake 0 li = []
monTake n [] = []
monTake n (x:xs) = x : monTake (n-1) xs

monDrop 0 li = li
monDrop n [] = []
monDrop n (x:xs) = monDrop (n-1) xs


longueurMaybe [] = Nothing
longueurMaybe li = Just (length li)

calculeLongueursLi li = map longueurMaybe li

diviser x 0 = Nothing
diviser x y = Just (x `div` y)

divEtPlus1 x y = fmap (+1) (diviser x y)

divplus1 x li = map (divEtPlus1 x) li
