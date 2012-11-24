data Nat = Z | S Nat
instance Show Nat where
    show = toRoman . toDecimal

toDecimal :: Nat -> Integer
toDecimal Z = 0
toDecimal (S n) = 1 + (toDecimal n)

toRoman :: Integer -> String
toRoman 0 = ""
toRoman n = b ++ toRoman (n - a)
	where (a,b) = head $ filter ((<= n) . fst) convMap
	--where (a,b) = head $ filter (\(a,_) -> a <= n) convMap


 -- TODO conversion map überarbeiten

convMap = [ (1000, "M")
		  , (999, "IM")
		  , (900, "CM")
		  , (500, "D")
		  , (499, "ID")
		  , (400, "CD")
		  , (100, "C")
		  , (99, "IC")
		  , (90, "XC")
		  , (50, "L")
		  , (49, "IL")
		  , (40, "XL")
		  , (10, "X")
		  , (9, "IX")
		  , (5, "V")
		  , (4, "IV")
		  , (1, "I") ]










toNat :: Int -> Nat
toNat n = foldr ($) Z $ replicate n S


































--data RatNumbers = Rat Numerator Denominator
--instance Show RatNumbers where
--    show (Rat n d)

--type Numerator = Nat
--type Denominator = Nat
--type NegaBinary = String

--toNegaBinary :: Int -> NegaBinary
--toNegaBinary = foldr (++) "" . reverse . map show . toNegaBinary'
--toNegaBinary' 0 = []
--toNegaBinary' n = abs (n `mod` radix) : toNegaBinary' (n `div'` radix)
--    where radix = (-2)

--div' :: Integral a => a -> a -> a
--div' a b
--    | a `mod` 2 == 1 = (a-1) `div` b
--    | otherwise      = a `div` b

--toDecimal :: NegaBinary -> Int
--toDecimal n = sum $ zipWith ((*) . digitToInt) (reverse n) values
--    where values = map ((-2)^) [0..]
