import Data.Char

data Nat = Z | S Nat
instance Show Nat where
    show Z = ""

toNat :: Int -> Nat
toNat n = foldr ($) Z $ replicate n S



data RatNumbers = Rat Numerator Denominator
instance Show RatNumbers where
    show (Rat n d)

type Numerator = Nat
type Denominator = Nat
type NegaBinary = String

toNegaBinary :: Int -> NegaBinary
toNegaBinary = foldr (++) "" . reverse . map show . toNegaBinary'
toNegaBinary' 0 = []
toNegaBinary' n = abs (n `mod` radix) : toNegaBinary' (n `div'` radix)
    where radix = (-2)

div' :: Integral a => a -> a -> a
div' a b
    | a `mod` 2 == 1 = (a-1) `div` b
    | otherwise      = a `div` b

toDecimal :: NegaBinary -> Int
toDecimal n = sum $ zipWith ((*) . digitToInt) (reverse n) values
    where values = map ((-2)^) [0..]
