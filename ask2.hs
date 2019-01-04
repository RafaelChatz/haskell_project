import Data.List
import System.IO

swap :: (Int,Int) -> (Int,Int)
swap (a,b)=(b,a)

removeItem :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
removeItem _ []     = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | (swap x)==y = removeItem x ys
                    | otherwise = y : removeItem x ys

nubtuple :: [(Int,Int)] -> [(Int,Int)]
nubtuple [] = []
nubtuple (x:xs) = let xn = (removeItem x xs) in (let xns = nubtuple xn in sort(x:xns))

tupleinorder :: [(Int,Int)] -> [(Int,Int)]
tupleinorder [] = []
tupleinorder ((x1,x2):xs) =let xn=(tupleinorder xs) in( if(x1<x2)
                         then ((x1,x2):xn)
                         else ((x2,x1):xn))

fixtuple::[(Int,Int)] -> [(Int,Int)]
fixtuple a = let b= nubtuple a in (tupleinorder b)

checktuple :: ([Int] ,[(Int,Int)] )-> Bool
checktuple (a,[])=True
checktuple (a,(b1,b2):bs) = if(b1 `elem` a)
                            then if(b2 `elem` a)
                                then checktuple (a,bs)
                                else False
                            else False

listlistlength :: [[Int]] -> [Int]
listlistlength a = map length a

listlistappend ::(Int,Int) -> [[Int]] -> [[Int]]
listlistappend (a1,a2) [] = [[a1,a2]]
listlistappend (a1,a2) (a:as) = if(or [(a1 `elem` a),(a2 `elem` a)] )
                                then if ( and [(a1 `elem` a) ,(not (a2 `elem` a) )] )
                                    then ((a2:a):as)
                                    else if ( and [(a2 `elem` a) ,(not (a1 `elem` a) )] )
                                    then ((a1:a):as)
                                    else (a:as)
                                else a:(listlistappend (a1,a2) as)

intToDouble :: Int -> Double
intToDouble x =fromIntegral x/1.0

doubleToInt :: Double -> Int
doubleToInt x =floor x

add1:: Int -> [Int]->[Int]
add1 0 b=b
add1 a b =let b1=add1 (a-1) b in (1:b1)

fdiv2 :: Int -> Double
fdiv2 x = intToDouble(x)/2

fdiv3 :: Int -> Double
fdiv3 x = intToDouble(x)/3

logx :: Double -> Double -> Double
logx x y= log x / log y

------------------------------------------------------------------------
-----multiples
ispowerx :: Double -> Double -> Bool
ispowerx x y = ceiling (logx x y )==floor(logx x y)

iscomp :: Int -> Bool
iscomp y = if (y `mod` 2 == 0)
           then iscomp (doubleToInt(fdiv2(y)))
           else if (y `mod` 3 == 0)
           then iscomp (doubleToInt(fdiv3(y)))
           else ispowerx (intToDouble(y)) 5

multi235 :: Int -> Bool
multi235 n
    | iscomp n = True
    | otherwise = False

multiples = [ n | n <- [1..], multi235 n]
------------------------------------------------------------------------
------ function for sssum
fun :: Int -> Int -> Int -> Int
fun a b c = a*b*c
------------------------------------------------------------------------
-----sssum
sssumn :: (Int -> Int -> Int -> Int) -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
sssumn f a b c a1 b1 c1 res= if (c1 < c)
                            then sssumn f a b c a1 b1 (c1+1) res +(f a1 b1 (c1+1))
                            else if (b1 < b)
                            then sssumn f a b c a1 (b1+1) 1 res +(f a1 (b1+1) 1)
                            else if (a1 < a)
                            then sssumn f a b c (a1+1) 1 1 res +(f (a1+1) 1 1)
                            else res

sssum :: (Int -> Int -> Int -> Int) -> Int -> Int -> Int -> Int
sssum f a b c = sssumn f a b c 1 1 1 (f 1 1 1)
------------------------------------------------------------------------
-----components
comp1::[(Int,Int)]  -> [[Int]]
comp1 [(b1,b2)]  = [[b1,b2]]
comp1 ((b1,b2):bs)  =let a=comp1 bs in (listlistappend (b1,b2) a)

comp :: [Int] -> [(Int,Int)] -> (Int, [Int])
comp a b = let bn = comp1 b  in (   (  ((length bn) + (length a) - (foldl (+) 0 (listlistlength bn) ) ) ,sort (add1 ((length a) - (foldl (+) 0 (listlistlength bn) )) (listlistlength bn) ) )  )

components :: ([Int] ,[(Int,Int)] )-> (Int,[Int])
components ([],[])= (0,[])
components (a,b) = if (checktuple (a,b))
                    then (let bs = fixtuple b in (comp a bs) )
                    else (-1,[-1])

------------------------------------------------------------------------
----fractions
calcfractions :: Int -> Int -> Int ->(Int,Int)
calcfractions a b c =let d =( (a*c) - b ) in (d,b*c)

findfraction :: Int -> Int -> [Int]
findfraction 1 q= [q]
findfraction p q= if (q `mod` p==0)
                  then findfraction 1 (q `div` p)
                  else let a = (ceiling ((intToDouble q)/(intToDouble p)) ) in ( let (pn,qn)=(calcfractions p q a) in a:(findfraction pn qn) )


fractions :: Int -> Int -> [Int]
fractions p q = if (p==1)
                then [q]
                else if (q `mod` p==0)
                    then [(q `div` p)]
                else findfraction p q

------------------------------------------------------------------------
-----ugliness
ugliness2 :: [Int]->[Int]->Int
ugliness2 [a] [b] = abs(a-b)
ugliness2 (a:as) (b:bs) =let n = ugliness2 as bs in ( if(n>abs(a-b))
                                                      then n
                                                      else abs(a-b) )

ugliness1 :: [Int]->[Int]->Int
--ugliness1 a (b:bs) =let b1= take(length (a)) (b:bs) ) in b1
ugliness1 a (b:bs) =if((length a)==(length (b:bs)))
                    then ugliness2 a (b:bs)
                    else let b1= take(length (a)) (b:bs)  in (let n=ugliness1 a bs in (let n1= ugliness2 a b1 in ( if (n>n1)
                                                                                                                    then n1
                                                                                                                    else n) ) )

ugliness :: [Int]->[Int]->Int
ugliness a b =let (a1,b1)=(sort a,sort b ) in if ((length a1)<(length b1))
                                              then ugliness1 a1 b1
                                              else if ((length a1)>(length b1))
                                              then ugliness1 b1 a1
                                              else ugliness2 a1 b1
------------------------------------------------------------------------
