module Nat
( Nat(Z,S)
, plusNat
, minusNat
, timesNat
, divNat
, grNat
, modNat
, leNat
, eqNat
, grEqNat
, leEqNat
) where

data Nat = Z | S Nat deriving (Show, Eq)

toNat :: Int -> Nat
toNat n = foldr ($) Z $ replicate n S

incr :: Nat -> Nat
incr Z = S Z
incr n = S n

decr :: Nat -> Nat
decr Z = Z
decr (S n) = n


plusNat :: Nat -> Nat -> Nat
plusNat Z n = n
plusNat n Z = n
plusNat n k = plusNat (incr n) (decr k)

minusNat :: Nat -> Nat -> Nat
minusNat Z _ = Z
minusNat n Z = n
minusNat n k = minusNat (decr n) (decr k)

timesNat :: Nat -> Nat -> Nat
timesNat Z _ = Z
timesNat n k = timesNat' n k Z
	where
 		timesNat' :: Nat -> Nat -> Nat -> Nat
 		timesNat' _ Z acc = acc
 		timesNat' n k acc = timesNat' n (decr k) (acc `plusNat` n)

divNat :: Nat -> Nat -> Nat
divNat _ Z = error "Invalid argument"
divNat Z _ = Z
divNat n k = divNat' n k Z
 	where
 		divNat' :: Nat -> Nat -> Nat -> Nat
 		divNat' n k acc
 			| n `grEqNat` k = divNat' (n `minusNat` k) k (incr acc)
 			| otherwise 	= acc

--n mod k = r = n - q*k = n - (n/k)*k
--	--> q := Quotient a b (a/b)

modNat :: Nat -> Nat -> Nat
modNat _ Z = error "Invalid argument"
modNat n k = n `minusNat` (q `timesNat` k)
 	where q = n `divNat` k


--Vergleichsoperationen:

eqNat :: Nat -> Nat -> Bool
eqNat Z Z = True
eqNat _ Z = False
eqNat Z _ = False
eqNat n k = eqNat (decr n) (decr k)

grNat :: Nat -> Nat -> Bool
grNat Z Z = False
grNat Z _ = False
grNat _ Z = True
grNat n k = grNat (decr n) (decr k)

leNat :: Nat -> Nat -> Bool
leNat Z Z = False
leNat Z _ = True
leNat _ Z = False
leNat n k = leNat (decr n) (decr k)

grEqNat :: Nat -> Nat -> Bool
grEqNat Z Z = True
grEqNat Z _ = False
grEqNat _ Z = True
grEqNat n k = grEqNat (decr n) (decr k)

leEqNat :: Nat -> Nat -> Bool
leEqNat Z Z = True
leEqNat Z _ = True
leEqNat _ Z = False
leEqNat n k = leEqNat (decr n) (decr k)
