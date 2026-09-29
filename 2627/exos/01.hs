firstsucc x y = succ x + succ y
infi = succ 1 + succ 2
pref1 = (+)(succ 1) (succ 2)

secondsucc x y z = succ x + succ y + succ z
infi2 = succ 1 + succ 2 + succ 3
pref2 = (+)((+)(succ 1) (succ 2)) (succ 3)

somme x y = x + y

double x = x*2

quadruple x = double (double x)

triple x = double x + x

triplet x = 3*x

produit x y = x*y

maxi x y = 
    if x > y
        then x
    else y
recurence n = mathv n 2

mathv x y =
    if x > 2
        then mathv (x-1) (3*y-1)
    else y

facto n = 
    if n == 0
        then 1
    else n*facto (n-1)

incrementerListe [] = []
incrementerListe (x:xs) = (x + 1) : incrementerListe xs

doublerListe [] = []
doublerListe (x:xs) = (x*2) : doublerListe xs

sumListe [] = 0
sumListe (x:xs) = x + sumListe xs

map1 = map double [2,9,3]

map2 = map (\x -> x*2) [4,5,8]

filter1 = filter (\x -> x > 10) [3,15,7,42,10,11]

foldl1 = foldl (*) 1 [4,6,10]

foldl2 = foldl (++) "" ["Hello", " ", "World"]
 
diviser x y =
    if y == 0
        then Nothing
        else Just (x / y)

fmap1 = fmap (\x -> x + 1) (diviser 4 5)

