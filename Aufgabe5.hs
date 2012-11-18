data Tree = Null | Tree Label Tree Tree deriving (Eq,Show)
type Label = Integer

-- variables for testing

f1 = \x y z -> x+y+z
f2 = \x y z -> x*y*z

t1 = Null
t2 = Tree 2 (Tree 3 Null Null) (Tree 5 Null Null)
t3 = Tree 2 (Tree 3 (Tree 5 Null Null) Null) (Tree 7 Null Null)

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

tfold :: (Label -> Label -> Label -> Label) -> Label -> Tree -> Label
tfold _ v Null = v
tfold f v (Tree label lc rc) = f label (tfold f v lc) (tfold f v rc)

main = do
	putStrLn . show $ tmap (+1) t1 == Null
	putStrLn . show $ tmap (+1) t2 ==
		Tree 3 (Tree 4 Null Null) (Tree 6 Null Null)
	putStrLn . show $ tmap (+1) t3 ==
		Tree 3 (Tree 4 (Tree 6 Null Null) Null) (Tree 8 Null Null)
	
	putStrLn . show $ tfold f1 0 t1 == 0
	putStrLn . show $ tfold f2 1 t1 == 1
	putStrLn . show $ tfold f1 0 t2 == 10
	putStrLn . show $ tfold f1 0 t3 == 17
	putStrLn . show $ tfold f2 1 t2 == 30
	putStrLn . show $ tfold f2 1 t3 == 210
