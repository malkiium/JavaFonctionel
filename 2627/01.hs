firstsucc x y = succ x + succ y
infi = succ 1 + succ 2
pref1 = (+)(succ 1) (succ 2)

secondsucc x y z = succ x + succ y + succ z
infi2 = succ 1 + succ 2 + succ 3
pref2 = (+)((+)(succ 1) (succ 2)) (succ 3)

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