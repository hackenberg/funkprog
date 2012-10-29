------------
-- Teil 1 --
------------
kgv :: Integer -> Integer -> Integer
kgv = lcm

------------
-- Teil 2 --
------------
agv :: Integer -> Integer -> (Integer,Integer) -> [Integer]
agv 0 _ _ = []
agv _ 0 _ = []
agv a b (x,y) = takeWhile (>=x) $ takeWhile (<=y) $ map ((kgv a b)*) [1..]

--------------
-- Database --
--------------
type PassName = String
type FlightNumber = Integer
type PlaceOfDeparture = String
type Destination = String
type Airfare = Integer
type Database = [(PassName,FlightNumber,PlaceOfDeparture,Destination,Airfare)]

data DataSet = DataSet { passName :: PassName
                       , flightNumber :: FlightNumber
                       , placeOfDeparture :: PlaceOfDeparture
                       , destination :: Destination
                       , airfare :: Airfare
                       } deriving (Show, Read)

parse :: (PassName,FlightNumber,PlaceOfDeparture,Destination,Airfare) -> DataSet
parse (a,b,c,d,e) = DataSet a b c d e

db =  [ ("Anton",857,"Vienna","London",237),
        ("Berta",456,"Paris","Berlin",278),
        ("Anton",123,"Rome","London",417),
        ("Anton",109,"London","Berlin",237),
        ("Karla",888,"Vienna","Rome",350),
        ("Karla",832,"Rome","London",417),
        ("Berta",857,"Vienna","London",199),
        ("Karla",753,"Vienna","London",237) ]

------------------------
-- Sortieralgorithmen --
------------------------
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort lesser ++ [x] ++ qsort greater
    where
        lesser = [ a | a <- xs, a <= x ]
        greater = [ a | a <- xs, a > x ]

qtsort :: Ord a => [(a,b)] -> [(a,b)]
qtsort [] = []
qtsort (x:xs) = qtsort lesser ++ [x] ++ qtsort greater
    where
        lesser = [ a | a <- xs, (fst a) <= (fst x) ]
        greater = [ a | a <- xs, (fst a) > (fst x) ]

------------
-- Teil 3 --
------------
flights :: Database -> PassName -> [(FlightNumber,Airfare)]
flights db name = qtsort [ (flightNumber x,airfare x) | x <- (map parse db), (passName x) == name ]

------------
-- Teil 4 --
------------
pass2Dest :: Database -> Destination -> [PassName]
pass2Dest db dest = qsort [ passName x | x <- (map parse db), (destination x) == dest ]

------------
-- Teil 5 --
------------
mostValuedPass :: Database -> PlaceOfDeparture -> Destination -> ([PassName],Airfare)
mostValuedPass db dep dest = (reverse $ qsort [ passName x | x <- connection, (airfare x) == maxAirfare ],maxAirfare)
    where
        connection = [ x | x <- (map parse db), (placeOfDeparture x) == dep, (destination x) == dest ]
        maxAirfare = foldl (max) 0 [ airfare x | x <- (map parse db), (placeOfDeparture x) == dep, (destination x) == dest ]
