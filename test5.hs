f1 = \x y z -> x+y+z
f2 = \x y z -> x*y*z

t1 = Null
t2 = Tree 2 (Tree 3 Null Null) (Tree 5 Null Null)
t3 = Tree 2 (Tree 3 (Tree 5 Null Null) Null) (Tree 7 Null Null)

tmap (+1) t1 == Null
tmap (+1) t2 == Tree 3 (Tree 4 Null Null) (Tree 6 Null Null)
tmap (+1) t3 == Tree 3 (Tree 4 (Tree 6 Null Null) Null) (Tree 8 Null Null)

tzw (+) t1 t2 == Null
tzw (+) t2 t3 == Tree 4 (Tree 6 Null Null) (Tree 12 Null Null)

tfold f1 0 t1 == 0
tfold f2 1 t1 == 1
tfold f1 0 t2 == 10
tfold f1 0 t3 == 17
tfold f2 1 t2 == 30
tfold f2 1 t3 == 210



db = Db [([Sc 'M' "Smith",Sc 'G' "Martin",Sc 'P' "Erdos"],"Newtonian Forms of Prime Factors"),
         ([Sc 'P' "Erdos",Sc 'W' "Reisig"],"Stuttering in Petri Nets") ,
         ([Sc 'M' "Smith",Sc 'X' "Chen"],"First Order Derivates in Structured Programming"),
         ([Sc 'T' "Jablonski",Sc 'Z' "Hsueh"],"Selfstabilizing Data Structures"),
         ([Sc 'X' "Chen",Sc 'L' "Li"],"Prime Numbers and Beyond")]

erdosNum db (Sc 'P' "Erdos") == 0
erdosNum db (Sc 'M' "Smith") == 1
erdosNum db (Sc 'X' "Chen") == 2
erdosNum db (Sc 'L' "Li") == 3
erdosNum db (Sc 'Z' "Hsueh") == (-1)
erdosNum db (Sc 'K' "Tochterle") == (-1)
