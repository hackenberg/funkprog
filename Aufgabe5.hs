module Aufgabe5
( Tree(Null,Tree)
, Label
, tmap
, tzw
, tfold
, ErdosNumber
, Scientist(Sc)
, Initial
, SurName
, Database(Db)
, Author
, PaperTitle
, erdosNum
, getCoAuthors
) where

import Data.List


 -- Part 1

data Tree = Null | Tree Label Tree Tree deriving (Eq,Show)
type Label = Integer


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


 -- Part 2

type ErdosNumber = Integer
data Scientist = Sc Initial SurName deriving (Eq)
type Initial = Char
type SurName = String
type Author = Scientist
newtype Database = Db [([Author],PaperTitle)]
type PaperTitle = String


getCoAuthors :: Database -> [Scientist] -> [Scientist]
getCoAuthors (Db []) _ = []
getCoAuthors _ [] = []
getCoAuthors (Db (([],t):es)) (s:ss) = []
getCoAuthors (Db ((as,t):es)) (s:ss) = cos
    where
        cos = [ sc | sc <- nubbed, sc `notElem` (s:ss) ]
        nubbed = nub (samecos ++ recss ++ reces)
        samecos = if s `elem` as then as else []
        recss = getCoAuthors (Db ((as,t):es)) ss
        reces = getCoAuthors (Db es) (s:ss)


erdosNum :: Database -> Scientist -> ErdosNumber
erdosNum _ (Sc 'P' "Erdos") = 0  -- Erdos himself
erdosNum (Db []) _          = -1 -- empty Database; should not happen
erdosNum db sc              = erdosNum1 db [sc] []

erdosNum1 :: Database -> [Scientist] -> [Scientist] -> ErdosNumber
erdosNum1 db list last
    | (Sc 'P' "Erdos") `elem` coAuthors = 1
    | list == last = -1
    | otherwise = if recursion == -1 then -1 else 1 + recursion
    where
        recursion = erdosNum1 db (nub (list ++ coAuthors)) list
        coAuthors = getCoAuthors db list
