> type NegaBinary = String

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
