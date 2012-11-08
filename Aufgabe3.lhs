> import Data.Char

> type NegaBinary = String

> radix = (-2)
> list = map show [0,1,110,111,100,101,11010,11011,11000,11001]

> posVal :: Integer -> [Integer]
> posVal n = map ((-2)^) [0..n]



Zuerst definiert man einige mathematische Hilfsfunktionen:
	-) div' funktioniert wie div, rundet aber immer zur null hin (verify!!!)
	-) expand wandelt eine Dezimalzahl in eine negabinäre zahl um
	-) toBase10 konvertiert von negabinär nach dezimal

> div' :: Integral a => a -> a -> a
> div' a b
>	| a `mod` 2 == 1 = (a-1) `div` b
>	| otherwise		 = a `div` b

> expand :: Int -> NegaBinary
> expand n = foldr (++) "" $ reverse . map show $ expand' n
> expand' 0 = []
> expand' n = abs (n `mod` radix) : expand' (n `div'` radix)

> toBase10 :: NegaBinary -> Int
> toBase10 n = sum $ zipWith ((*) . digitToInt) (reverse n)  posVal
> 	where posVal = map ((-2)^) [0..]



Teil 1: extract

> extract :: String -> NegaBinary
> extract [] = []
> extract (x:xs)
> 	| not ('0' `elem` (x:xs)) && not ('1' `elem` (x:xs)) = "0"
> 	| otherwise = extract' (x:xs)
> extract' :: String -> NegaBinary
> extract' (x:xs)
>	| x `elem` ['0','1'] = x : extract xs
>	| otherwise = extract' xs

Teil 2:

> nbIncr :: NegaBinary -> NegaBinary
> nbIncr = expand . (+1) . toBase10













 roundToZero :: (RealFrac a, Integral b) => a -> b
 roundToZero d
 	| d > 0 = floor d
 	| d < 0 = ceiling d
 	| otherwise = 0
