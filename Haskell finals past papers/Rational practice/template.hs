import GHC.Real
import Data.List

makeFraction :: Integer -> Integer -> Rational
makeFraction a b = a % b

myRational :: Rational
myRational = 3 :% 4

isZero :: Rational -> Bool
isZero (0:%d) = True
isZero _ = False

isNegative :: Rational -> Bool
isNegative (a:%b) = (a < 0)

gcd' :: Integer -> Integer -> Integer
gcd' 0 b = b
gcd' a 0 = a
gcd' a b = gcd' b (snd (a `quotRem` b)) 

f1 :: Rational -> Rational
f1 a = a^2 - 3*a +2

f1' :: Rational -> Rational
f1' a = 2*a - 3

rounded :: Rational -> Integer
rounded r
  | isNegative r = ceiling (r - 1/2)
  | otherwise = floor (r + 1/2)

approxReal :: Double -> Integer -> [Rational]
approxReal value p = [ (rounded ((toRational value) * fromIntegral x)) % x | x <- [1..p]]

f :: Int -> (Int, Int, Int) 
f a = (a, k, p')
  where
    (k,p') = f' (10, 3)
    f' (0,d) = (0,d)
    f' (p, a') = f' (p-1, a)
      where 
        a = a' * 6