> import Data.Char
> import Data.Bits

> type NegaBinary = String

> radix = (-2)
> list = map show [0,1,110,111,100,101,11010,11011,11000,11001]

> posVal :: Integer -> [Integer]
> posVal n = map ((-2)^) [0..n]



TODO: 0 -> ""; Funktionen für negative NegaBinarys testen

Zuerst definiert man einige mathematische Hilfsfunktionen:
	-) div' funktioniert wie div, rundet aber immer zur null hin (verify!!!)
	-) expand wandelt eine Dezimalzahl in eine negabinäre zahl um
	-) toBase10 konvertiert von negabinär nach dezimal

> div' :: Integral a => a -> a -> a
> div' a b
>	| a `mod` 2 == 1 = (a-1) `div` b
>	| otherwise		 = a `div` b

> expand :: Int -> NegaBinary
> expand = foldr (++) "" . reverse . map show . expand'
> expand' 0 = []
> expand' n = abs (n `mod` radix) : expand' (n `div'` radix)

> toBase10 :: NegaBinary -> Int
> toBase10 n = sum $ zipWith ((*) . digitToInt) (reverse n)  posVal
> 	where posVal = map ((-2)^) [0..]



> extract :: String -> NegaBinary
> extract [] = []
> extract (x:xs)
> 	| not ('0' `elem` (x:xs)) && not ('1' `elem` (x:xs)) = "0"
> 	| otherwise = extract' (x:xs)
> extract' :: String -> NegaBinary
> extract' (x:xs)
>	| x `elem` ['0','1'] = x : extract xs
>	| otherwise = extract' xs

> nbIncr :: NegaBinary -> NegaBinary
> --nbIncr = expand . (+1) . toBase10
> nbIncr nb = nbIncr' nb 0 0
> nbIncr' :: NegaBinary -> Bool -> Bool -> NegaBinary
> nbIncr' nb c1 c2
> 	| last nb == '0' && c1 && c2 = (init nb) ++ "1"
> 	| last nb == '1' && c1 && c2 =
> 	| last nb == '0' && c1 && not c2 =
> 	| last nb == '1' && c1 && not c2 =
> 	| last nb == '0' && not c1 && c2 =
> 	| last nb == '1' && not c1 && c2 =
> 	| otherwise 	 = (nbIncr (init nb) 0) ++ "0"

> nbDecr :: NegaBinary -> NegaBinary
> nbDecr = expand . (flip (-) 1) . toBase10

> nbAbs :: NegaBinary -> NegaBinary
> nbAbs = expand . abs . toBase10

> nbPlus :: NegaBinary -> NegaBinary -> NegaBinary
> nbPlus nb "0" = nb
> nbPlus "0" nb = nb
> nbPlus nb "" = nb
> nbPlus "" nb = nb
> --nbPlus nb mb = nbPlus (nbIncr nb) (nbDecr mb)

 nbPlus a b = nbPlus' a b 0 0
 	where nbPlus' a b carry c = 








 roundToZero :: (RealFrac a, Integral b) => a -> b
 roundToZero d
 	| d > 0 = floor d
 	| d < 0 = ceiling d
 	| otherwise = 0
