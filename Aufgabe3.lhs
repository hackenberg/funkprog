> import Data.Char
> type NegaBinary = String
> list = map show [0,1,110,111,100,101,11010,11011,11000,11001]

> posVal :: Integer -> [Integer]
> posVal n = map ((-2)^) [0..n]

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
Zuerst benötigen wir eine Funktion, die ein NegaBinary in eine Zahl zur Basis 10
umwandelt. Diese sollte nach Möglichkeit kein String mehr sein.

> toBase10 :: NegaBinary -> Int
> toBase10 n = sum $ zipWith ((*) . digitToInt) (reverse n)  posVal
> 	where posVal = map ((-2)^) [0..]

fromBase10 :: Integer -> NegaBinary

> nbIncr :: NegaBinary -> NegaBinary
> nbIncr n
> 	| last n == '0' = init n ++ "1"
> 	| otherwise = error "not yet defined"
