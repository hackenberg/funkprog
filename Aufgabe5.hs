data Tree = Null | Tree Label Tree Tree deriving (Eq,Show)
type Label = Integer

 -- tmap :: (Label -> Label) -> Tree -> Tree
 -- tzw :: (Label -> Label -> Label) -> Tree -> Tree -> Tree
 -- tfold :: (Label -> Label -> Label -> Label) -> Label -> Tree -> Label

 -- first we specify the Tree traversals

inorder :: Tree -> [Label]
inorder Null = []
inorder (Tree label lc rc) =  (inorder lc) ++ [label] ++ (inorder rc)

preorder :: Tree -> [Label]
preorder Null = []
preorder (Tree label lc rc) = [label] ++ (preorder lc) ++ (preorder rc)

postorder :: Tree -> [Label]
postorder Null = []
postorder (Tree label lc rc) = [label] ++ (postorder lc) ++ (postorder rc)

isLeaf :: Tree -> Maybe Bool
isLeaf Null = Nothing
isLeaf (Tree _ Null Null) = Just True
isLeaf (Tree _ _ _) = Just False


tmap :: (Label -> Label) -> Tree -> Tree
tmap _ Null = Null
tmap f (Tree label lc rc) = Tree (f label) (tmap f lc) (tmap f rc)

tzw :: (Label -> Label -> Label) -> Tree -> Tree -> Tree
tzw _ Null Null = Null
tzw f (Tree l1 lc1 rc1) (Tree l2 lc2 rc2) =
	Tree (f l1 l2) (tzw f lc1 lc2) (tzw f rc1 rc2)
tzw _ _ _ = Null

tfold :: (Label -> Label -> Label -> Label) -> Label -> Tree -> Label
tfold _ v Null = v
tfold f v (Tree label lc rc) = f label (tfold f v lc) (tfold f v rc)



 -- test cases

f1 = \x y z -> x+y+z
f2 = \x y z -> x*y*z

t1 = Null
t2 = Tree 2 (Tree 3 Null Null) (Tree 5 Null Null)
t3 = Tree 2 (Tree 3 (Tree 5 Null Null) Null) (Tree 7 Null Null)

main = do

	putStrLn " -- test cases"
	putStr "tmap (+1) t1 | "
	putStrLn . show $ tmap (+1) t1 == Null

	putStr "tmap (+1) t2 | "
	print $ tmap (+1) t2 == Tree 3 (Tree 4 Null Null) (Tree 6 Null Null)

	putStr "tmap (+1) t3 | "
	putStrLn . show $ tmap (+1) t3 ==
		Tree 3 (Tree 4 (Tree 6 Null Null) Null) (Tree 8 Null Null)

	putStr "tzw (+) t1 t2 | "
	print $ tzw (+) t1 t2 == Null

	putStr "tzw (+) t2 t3 | "
	print $ tzw (+) t2 t3 == Tree 4 (Tree 6 Null Null) (Tree 12 Null Null)
	
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
