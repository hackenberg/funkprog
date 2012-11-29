module Erdos
( ErdosNumber
, Scientist(Sc)
, Initial
, SurName
, Database(Db)
, Author
, PaperTitle
, erdosNum
, coAuthors
, coAuthors1
, rmDup
) where


type ErdosNumber = Integer

data Scientist = Sc Initial SurName deriving (Eq,Show) -- Eq kann durch eigene Methode ersetzt werden
type Initial = Char
type SurName = String

newtype Database = Db [( [Author], PaperTitle )] deriving (Show)
type Author = Scientist
type PaperTitle = String


coAuthors :: Author -> Database -> [Author]
coAuthors _ (Db []) = [] 
coAuthors a (Db (x:xs))
    | a `elem` (fst x) = (filter (/=a) $ fst x) ++ (coAuthors a (Db xs))
    | otherwise        = coAuthors a (Db xs)

coAuthors1 :: Database -> [Author] -> [Author]
coAuthors1 (Db []) _ = []
coAuthors1 _ [] = []
coAuthors1 db (x:xs) = rmDup $ (coAuthors x db) ++ (coAuthors1 db xs)


erdosNum :: Database -> Scientist -> ErdosNumber
erdosNum _ (Sc 'P' "Erdos") = 0  -- Erdos himself
erdosNum (Db []) _          = -1 -- empty Database; should not happen
erdosNum db sc              = erdosNum1 db [sc]
--erdosNum db sc = minimum [ erdosNum db x | x <- (coAuthors sc db) ]
--erdosNum db sc = (minimum (map (erdosNum db) (coAuthors sc db))) +1

erdosNum1 :: Database -> [Scientist] -> ErdosNumber
erdosNum1 db list
    | erdos `elem` coAuthors1 db list = 1
    | list == coAuthors1 db list = -1
    | otherwise = 1 + (minimum [ erdosNum db x | x <- (coAuthors1 db list) ])
 


erdos = (Sc 'P' "Erdos")

rmDup :: Eq a => [a] -> [a]
rmDup [] = []
rmDup (x:xs) = x : rmDup (filter (\y -> x /= y) xs)
