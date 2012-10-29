-- Aufgabe1.hs
-- Punkte: 75/100

-- Teil 1
-- Anm.: (-20) Testfälle prüfen kat (n-1) ?!?
katNumber :: Integer -> Integer
katNumber 0 = error "Argument ungueltig"
katNumber x = binom (2*n) n `div` (n+1)
    where
    	n = x - 1
        binom _ 0 = 1
        binom 0 _ = 0
        binom n k = n * binom (n-1) (k-1) `div` k

-- Teil 2
sumPowers :: Integer -> Integer -> Integer
sumPowers n k
    | k < 0     = -1
    | otherwise =  sum $ map (^k) [1..n]

-- Teil 3
shrink :: Char -> String -> String
shrink c (x:xs:xss)
    | xss == [] && x == c && xs == c = xs:xss
    | xss == [] && x /= c            = x:xs:xss
    | x == c && xs == c              = shrink c (xs:xss)
    | otherwise                      = x : (shrink c (xs:xss))

-- Teil 4
-- Anm: (-5) Fall leere Liste vergessen
stretch :: Char -> Integer -> String -> String
stretch _ _ [] = ""
stretch c n (x:[])
    | x == c = replicate (fromInteger n) x
    | otherwise = [x]
stretch c n (x:xs)
    | x == c && n < 0  = stretch c n xs
    | x == c && n >= 0 = (replicate (fromInteger n) x) ++ (stretch c n xs)
    | otherwise = x : (stretch c n xs)
