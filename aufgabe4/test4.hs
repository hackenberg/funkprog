plusNat (S (S Z)) (S (S (S Z))) == S (S (S (S (S Z))))
minusNat (S (S Z)) (S (S (S Z))) == Z
timesNat (S (S Z)) (S (S (S Z))) == S (S (S (S (S (S Z)))))
divNat (S (S Z)) (S (S (S Z))) == Z
modNat (S (S Z)) (S (S (S Z))) == S (S Z)
eqNat (S (S Z)) (S (S (S Z))) == False
grNat (S (S Z)) (S (S (S Z))) == False
leNat (S (S Z)) (S (S (S Z))) == True
grEqNat (S (S Z)) (S (S (S Z))) == False
leEqNat (S (S Z)) (S (S (S Z))) == True

mkCan (S (S Z),S (S (S Z))) == (Z,S Z)
plusNP (S Z,S (S (S Z))) (S (S (S (S Z))),S Z) == (S Z,Z)
minusNP (S Z,S (S (S Z))) (S (S (S (S Z))),S Z) == (Z,S (S (S (S (S Z)))))
timesNP (S Z,S (S (S Z))) (S (S (S (S Z))),S Z) == (Z,S (S (S (S (S (S Z))))))
divNP (S Z,S (S (S Z))) (S (S (S (S Z))),S Z) == (Z,S Z)
modNP (S Z,S (S (S Z))) (S (S (S (S Z))),S Z) == (S Z,Z)
eqNP (S Z,S (S (S Z))) (S (S (S (S Z))),S Z) == False
grNP (S Z,S (S (S Z))) (S (S (S (S Z))),S Z) == False
leNP (S Z,S (S (S Z))) (S (S (S (S Z))),S Z) == True
grEqNP (S Z,S (S (S Z))) (S (S (S (S Z))),S Z) == False
leEqNP (S Z,S (S (S Z))) (S (S (S (S Z))),S Z) == True
