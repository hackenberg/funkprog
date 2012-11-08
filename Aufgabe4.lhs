> Z = 0
> S = (+1)
> data Nat = Z | S Nat deriving Show
>
> plusNat :: Nat -> Nat -> Nat
> plusNat n k = plusNat (S n) k
>

minusNat :: Nat -> Nat -> Nat
timesNat :: Nat -> Nat -> Nat
divsNat :: Nat -> Nat -> Nat
modNat :: Nat -> Nat -> Nat
