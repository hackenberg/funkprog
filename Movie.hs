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
, get_atr
, upd_dbgri
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

 -- Liefere alle Regisseure zusammen mit den Filmtiteln und Erscheinungsjahr:
get_rtd :: Database -> [(Regisseur,Title,ReleaseDate)]
get_rtd = map (\(a,b,c,d,e,f) -> (b,a,d))

 -- Liefere alle Regisseure zusammen mit den Filmtiteln und dem Genre, die
 -- zugleich (einer der) Hauptdarsteller in diesem Film waren:
get_rtg :: Database -> [(Regisseur,Title,Genre)]    -- TODO
get_rtg = map (\(a,b,c,d,e,f) -> (b,a,e))

 -- Liefere die Titel aller Filme zusammen mit den Listen ihrer
 -- Hauptdarsteller, die im Jahr n erschienen sind:
get_tad :: Database -> ReleaseDate -> [(Title,MainActors,ReleaseDate)]
get_tad db n = map tad inYear
    where
        inYear = filter ((n==) . year $) db
        tad    = \(a,b,c,d,e,f) -> (a,c,d)

 -- Liefere die Liste aller Filme zusammen mit dem Erscheinungsjahr, in denen
 -- Schauspieler s einer der Hauptdarsteller war:
get_atr :: Database -> Actor -> [(Actor,Title,ReleaseDate)]
get_atr db s = map atr withActor
    where
        withActor = filter ((s `elem`) . actors $) db
        atr       = \(a,b,c,d,e,f) -> (s,a,d)

 -- Verändere die Preise aller Filme des Genres g, in denen Regisseur r Regie
 -- geführt hat um x. Für positives x führt dies zu einer Erhöhung, für
 -- negatives x zu einer Erniedrigung. Ist der sich so ergebende neue Preis
 -- kleiner oder gleich 0, wird er auf 1 gesetzt:
upd_dbgri :: Database -> Genre -> Regisseur -> Int -> Database
upd_dbgri [] _ _ _ = []
upd_dbgri (x:xs) g r n 
    | (g == genre x) && (r == regisseur x) = inc : (upd_dbgri xs g r n)
    | otherwise                            = x : (upd_dbgri xs g r n)
    where
        inc = (title x, regisseur x, actors x, year x, genre x, newPrice)
        newPrice = if ((price x) + n) > 0 then (price x) + n else 1

 -- Lösche alle Filme, in denen Schauspieler s einer der Hauptdarsteller war
 -- und die im Jahr j oder später erschienen sind:
upd_dbad :: Database -> Actor -> ReleaseDate -> Database
upd_dbad [] _ _     = []
upd dbad (x:xs) s j | (j <= year x) && (s `elem` actors x) = upd_dbad xs s j
                    | otherwise = x : (upd_dbad xs s j)

 -- Liefere alle Filme, die im Jahr j oder früher erschienen sind und in denen
 -- Schauspieler s nicht unter den Hauptdarstellern war:

 -- Sortiere die Datenbank absteigend nach Erscheinungsjahr, d.h. neuere Filme
 -- vor älteren Filmen. Sind in einem Jahr mehrere Filme erschienen, ist die
 -- Reihenfolge unerheblich, in der diese Filme nach der Sortierung aufgeführt
 -- sind:
