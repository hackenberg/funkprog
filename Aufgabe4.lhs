Natürliche Zahlen:

> data Nat = Z | S Nat deriving Show

toNat ist eine Hilfsfunktion zum debuggen.

> toNat :: Int -> Nat
> toNat n = foldr ($) Z $ replicate n S

incr liefert den Successor einer natürlichen Zahl.
decr liefert den predecessor einer natürlichen Zahl != 0

> incr :: Nat -> Nat
> incr Z = S Z
> incr n = S n

> decr :: Nat -> Nat
> decr Z = Z
> decr (S n) = n


Arithmetik:

plusNat und minusNat verwenden die Funktionen incr und decr,
timesNat benötigt zusätzlich einen Accumulator um Zwischenergebnisse
zu speichern.

> plusNat :: Nat -> Nat -> Nat
> plusNat Z n = n
> plusNat n Z = n
> plusNat n k = plusNat (incr n) (decr k)

> minusNat :: Nat -> Nat -> Nat
> minusNat Z _ = Z
> minusNat n Z = n
> minusNat n k = minusNat (decr n) (decr k)

> timesNat :: Nat -> Nat -> Nat
> timesNat Z _ = Z
> timesNat n k = timesNat' n k Z
> 	where
> 		timesNat' :: Nat -> Nat -> Nat -> Nat
> 		timesNat' _ Z acc = acc
> 		timesNat' n k acc = timesNat' n (decr k) (acc `plusNat` n)

> divsNat :: Nat -> Nat -> Nat
> divsNat _ Z = error "Invalid argument"
> divsNat Z _ = Z
> divsNat n k = divsNat' n k Z
> 	where
> 		divsNat' :: Nat -> Nat -> Nat -> Nat
> 		divsNat' n k acc
> 			| n `grEqNat` k = divsNat' (n `minusNat` k) k (incr acc)
> 			| otherwise 	= acc

n mod k = r = n - q*k = n - (n/k)*k
	--> q := Quotient a b (a/b)

> modNat :: Nat -> Nat -> Nat
> modNat _ Z = error "Invalid argument"
> modNat n k = n `minusNat` (q `timesNat` k)
> 	where q = n `divsNat` k


Vergleichsoperationen:

> eqNat :: Nat -> Nat -> Bool
> eqNat Z Z = True
> eqNat _ Z = False
> eqNat Z _ = False
> eqNat n k = eqNat (decr n) (decr k)

> grNat :: Nat -> Nat -> Bool
> grNat Z Z = False
> grNat Z _ = False
> grNat _ Z = True
> grNat n k = grNat (decr n) (decr k)

> leNat :: Nat -> Nat -> Bool
> leNat Z Z = False
> leNat Z _ = True
> leNat _ Z = False
> leNat n k = leNat (decr n) (decr k)

> grEqNat :: Nat -> Nat -> Bool
> grEqNat Z Z = True
> grEqNat Z _ = False
> grEqNat _ Z = True
> grEqNat n k = grEqNat (decr n) (decr k)

> leEqNat :: Nat -> Nat -> Bool
> leEqNat Z Z = True
> leEqNat Z _ = True
> leEqNat _ Z = False
> leEqNat n k = leEqNat (decr n) (decr k)


Ganze Zahlen:

> type NatPair = (Nat,Nat)

> mkCan :: NatPair -> NatPair
> mkCan (n,m)
> 	| n `grEqNat` m = ((n `minusNat` m),Z)
> 	| otherwise = (Z,(m `minusNat` n))

isNegative liefert True wenn die Zahl ein negatives VZ hat.
0 hat kein negatives VZ.

> isNegative :: NatPair -> Bool
> isNegative np
>	| show ((snd . mkCan) np) == (show Z) = False
>	| show ((fst . mkCan) np) == (show Z) = True
>	| otherwise = error "this is not possible"


Arithmetik:

> plusNP :: NatPair -> NatPair -> NatPair
> plusNP (n,m) (k,l) = (n `plusNat` k,m `plusNat` l)

> minusNP :: NatPair -> NatPair -> NatPair
> minusNP (n,m) (k,l) = (n `plusNat` l,m `plusNat` k)

> timesNP :: NatPair -> NatPair -> NatPair
> timesNP (Z,Z) _ = (Z,Z)
> timesNP _ (Z,Z) = (Z,Z)
> timesNP n m  = timesNP' (mkCan n) (mkCan m)
> 	where
> 		timesNP' :: NatPair -> NatPair -> NatPair
> 		timesNP' n m
> 			| (isNegative n) && (isNegative m) = ((snd n) `timesNat` (snd m),Z)
> 			| (not (isNegative n))&&(not (isNegative m)) = ((fst n) `timesNat` (fst m),Z)
> 			| (isNegative n) && (not (isNegative m)) = (Z,snd n `timesNat` fst m)
> 			| (not (isNegative n)) && (isNegative m) = (Z,fst n `timesNat` snd m)

divNP :: NatPair -> NatPair -> NatPair
modNP :: NatPair -> NatPair -> NatPair


Vergleichsoperationen:

> eqNP :: NatPair -> NatPair -> Bool
> eqNP np1 np2
> 	| (show . mkCan $ np1) == (show . mkCan $ np2) = True
> 	| otherwise = False

> grNP :: NatPair -> NatPair -> Bool
> grNP np1 np2
> -- 	| ((fst np1) `minusNat` (snd np1)) `grNat` ((fst np2) `minusNat` (snd np2)) = True
> 	| (show . fst . mkCan $ np1 `minusNP` np2) /= "Z" = True -- np1 - np2 = (>0)
> 	| otherwise = False

> leNP :: NatPair -> NatPair -> Bool
> leNP np1 np2
> 	| eqNP np1 np2 = False
> 	| grNP np1 np2 = False
> 	| otherwise = True

> grEqNP :: NatPair -> NatPair -> Bool
> grEqNP np1 np2 
> 	| leNP np1 np2 = False
> 	| otherwise = True

> leEqNP :: NatPair -> NatPair -> Bool
> leEqNP np1 np2
> 	| grEqNP np1 np2 = False
> 	| otherwise = True
