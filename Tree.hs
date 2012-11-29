module Tree
( Tree(Null,Tree)
, Label
, tmap
, tzw
, tfold
) where

data Tree = Null | Tree Label Tree Tree deriving (Eq,Show)
type Label = Integer

tmap :: (Label -> Label) -> Tree -> Tree
tmap _ Null = Null
tmap f (Tree l left right) = Tree (f l) (tmap f left) (tmap f right)

tzw :: (Label -> Label -> Label) -> Tree -> Tree -> Tree
tzw _ Null Null = Null
tzw f (Tree l1 lc1 rc1) (Tree l2 lc2 rc2) =
	Tree (f l1 l2) (tzw f lc1 lc2) (tzw f rc1 rc2)
tzw _ _ _ = Null

tfold :: (Label -> Label -> Label -> Label) -> Label -> Tree -> Label
tfold _ v Null = v
tfold f v (Tree label lc rc) = f label (tfold f v lc) (tfold f v rc)
