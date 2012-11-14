data Tree = Null | Tree Label Tree Tree deriving (Eq,Show)
type Label = Integer

-- tmap :: (Label -> Label) -> Tree -> Tree
-- tzw :: (Label -> Label -> Label) -> Tree -> Tree -> Tree
-- tfold :: (Label -> Label -> Label -> Label) -> Label -> Tree -> Label
