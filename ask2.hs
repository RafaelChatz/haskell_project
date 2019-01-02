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
tupleinorder ((x1,x2):xs) =let xn=(kalaeimai xs) in( if(x1<x2)
                         then ((x1,x2):xn)
                         else ((x2,x1):xn))

intToDouble :: Int -> Double
intToDouble x =fromIntegral x/1.0

doubleToInt :: Double -> Int
doubleToInt x =floor x

fdiv2 :: Int -> Double
fdiv2 x = intToDouble(x)/2

fdiv3 :: Int -> Double
fdiv3 x = intToDouble(x)/3

logx :: Double -> Double -> Double
logx x y= log x / log y

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


fun :: Int -> Int -> Int -> Int
fun a b c = a^b^c

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

checktuple :: ([Int] ,[(Int,Int)] )-> Bool
checktuple (a,[])=True
checktuple (a,(b1,b2):bs) = if(b1 `elem` a)
                              then if(b2 `elem` a)
                                   then checktuple (a,bs)
                                   else False
                              else False


--comp :: [Int] -> [(Int,Int)] -> (Int, [Int])
--comp a ((b1,b2):bs) = Let b =
--comp a ((b1,b2):bs) =let len=length bs in (if (len==0)
--                                           then let (c,[d])=(1,[2]) in (c,[d])
--                                           else (2,[121,1])
--                                           )

--components :: ([Int] ,[(Int,Int)] )-> (Int,[Int])
--components ([],_)= (0,[])
--components (a,b) = let as=nubtuple b in (if ((length as)==0)
--                                        then (0,[])
--                                        else (if (checktuple (a,b))
--                                             then comp a b
--                                             else (0,[])
--                                             )
--                                        )
