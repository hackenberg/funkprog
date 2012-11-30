import Tree
import Erdos

f1 = \x y z -> x+y+z
f2 = \x y z -> x*y*z

t1 = Null
t2 = Tree 2 (Tree 3 Null Null) (Tree 5 Null Null)
t3 = Tree 2 (Tree 3 (Tree 5 Null Null) Null) (Tree 7 Null Null)

db = Db [([Sc 'M' "Smith",Sc 'G' "Martin",Sc 'P' "Erdos"],
           "Newtonian Forms of Prime Factors"),
         ([Sc 'P' "Erdos",Sc 'W' "Reisig"],
           "Stuttering in Petri Nets") ,
         ([Sc 'M' "Smith",Sc 'X' "Chen"],
           "First Order Derivates in Structured Programming"),
         ([Sc 'T' "Jablonski",Sc 'Z' "Hsueh"],
           "Selfstabilizing Data Structures"),
         ([Sc 'X' "Chen",Sc 'L' "Li"],
           "Prime Numbers and Beyond")]


main = do

     -- tmap
	putStr ("tmap (+1) t1 = " ++ (show $ tmap (+1) t1) ++ " | ")
	print $ tmap (+1) t1 == Null

	putStr ("tmap (+1) t2 = " ++ (show $ tmap (+1) t2) ++ " | ")
	print $ tmap (+1) t2 == Tree 3 (Tree 4 Null Null) (Tree 6 Null Null)

	putStr ("tmap (+1) t3 = " ++ (show $ tmap (+1) t3) ++ " | ")
	print $ tmap (+1) t3 ==
            Tree 3 (Tree 4 (Tree 6 Null Null) Null) (Tree 8 Null Null)

     -- tzw

	putStr "tzw (+) t1 t2 | "
	print $ tzw (+) t1 t2 == Null

	putStr "tzw (+) t2 t3 | "
	print $ tzw (+) t2 t3 == Tree 4 (Tree 6 Null Null) (Tree 12 Null Null)

     -- tfold
	
	putStr "tfold f1 0 t1 | "
	putStrLn . show $ tfold f1 0 t1 == 0

	putStr "tfold f2 1 t1 | "
	putStrLn . show $ tfold f2 1 t1 == 1

	putStr "tfold f1 0 t2 | "
	putStrLn . show $ tfold f1 0 t2 == 10

	putStr "tfold f1 0 t3 | "
	putStrLn . show $ tfold f1 0 t3 == 17

	putStr "tfold f2 1 t2 | "
	putStrLn . show $ tfold f2 1 t2 == 30

	putStr "tfold f2 1 t3 | "
	putStrLn . show $ tfold f2 1 t3 == 210

	putStrLn . show $ erdosNum db (Sc 'P' "Erdos")
	putStrLn . show $ erdosNum db (Sc 'M' "Smith")
	putStrLn . show $ erdosNum db (Sc 'X' "Chen")
	putStrLn . show $ erdosNum db (Sc 'L' "Li")
	putStrLn . show $ erdosNum db (Sc 'Z' "Hsueh")
	putStrLn . show $ erdosNum db (Sc 'K' "Toechterle")
