import Data.List
import System.IO

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
fun a b c = 1

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
