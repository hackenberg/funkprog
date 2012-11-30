module Erdos
( ErdosNumber
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

type ErdosNumber = Integer
data Scientist   = Sc Initial SurName deriving (Eq)
type Initial     = Char
type SurName     = String
newtype Database = Db [( [Author], PaperTitle )]
type Author      = Scientist
type PaperTitle  = String


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
