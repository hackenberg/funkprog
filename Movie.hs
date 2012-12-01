module Movie
( Movie
, Title
, Regisseur
, Actor
, MainActors
, ReleaseDate
, Genre(Thriller,Fantasy,ScienceFiction,Comedy)
, SalesPrice
, Database
, title
, regisseur
, actors
, year
, genre
, price
, rmDup
, contains
, get_rtd
, get_rtg
, get_tad
) where


type Movie = (Title,Regisseur,MainActors,ReleaseDate,Genre,SalesPrice)
type Title = String
type Regisseur = String
type Actor = String
type MainActors = [Actor]
type ReleaseDate = Int
data Genre = Thriller | Fantasy | ScienceFiction | Comedy deriving (Eq,Ord,Show)
type SalesPrice = Int
type Database = [Movie]

 -- get Funktionen
title :: Movie -> Title
title (a,b,c,d,e,f) = a

regisseur :: Movie -> Regisseur
regisseur (a,b,c,d,e,f) = b

actors :: Movie -> MainActors
actors (a,b,c,d,e,f) = c

year :: Movie -> ReleaseDate
year (a,b,c,d,e,f) = d

genre :: Movie -> Genre
genre (a,b,c,d,e,f) = e

price :: Movie -> SalesPrice
price (a,b,c,d,e,f) = f


 -- Hilfsfunktion zum Prüfen ob ein Film in einer Datenbank enthalten ist
contains :: Database -> Movie -> Bool
contains [] _       = False
contains (x:xs) mov | x == mov  = True
                    | otherwise = contains xs mov

 -- Entfernt Duplikate aus einer Datenbank. Die Reihenfolger der MainActors ist
 -- dabei egal.
rmDup :: Database -> Database
rmDup []     = []
rmDup (x:xs) | xs `contains` x = (rmDup xs)
             | otherwise       = x : (rmDup xs)


get_rtd :: Database -> [(Regisseur,Title,ReleaseDate)]
get_rtd = map (\(a,b,c,d,e,f) -> (b,a,d))

get_rtg :: Database -> [(Regisseur,Title,Genre)]
get_rtg = map (\(a,b,c,d,e,f) -> (b,a,e))

get_tad :: Database -> ReleaseDate -> [(Title,MainActors,ReleaseDate)]
get_tad db n = map tad inYear
    where
        inYear = filter ((n==) . year $) db
        tad  = \(a,b,c,d,e,f) -> (a,c,d)
