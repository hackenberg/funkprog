kgv :: Integer -> Integer -> Integer
kgv a b = lcm a b

agv :: Integer -> Integer -> (Integer,Integer) -> [Integer]
agv 0 _ _ = []
agv _ 0 _ = []
agv a b (x,y) = takeWhile (>=x) $ takeWhile (<=y) $ map ((kgv a b)*) [1..]

db =  [ ("Anton",857,"Vienna","London",237),
        ("Berta",456,"Paris","Berlin",278),
        ("Anton",123,"Rome","London",417),
        ("Anton",109,"London","Berlin",237),
        ("Karla",888,"Vienna","Rome",350),
        ("Karla",832,"Rome","London",417),
        ("Berta",857,"Vienna","London",199),
        ("Karla",753,"Vienna","London",237) ]


type PassName = String
type FlightNumber = Integer
type PlaceOfDeparture = String
type Destination = String
type Airfare = Integer
type Database = [(PassName,FlightNumber,PlaceOfDeparture,Destination,Airfare)]
type Entry = (PassName,FlightNumber,PlaceOfDeparture,Destination,Airfare)

qtsort :: Ord a => [(a,b)] -> [(a,b)]
qtsort [] = []
qtsort (x:xs) = qtsort lesser ++ [x] ++ qtsort greater
    where
        lesser = [ a | a <- xs, (fst a) <= (fst x) ]
        greater = [ a | a <- xs, (fst a) >= (fst x) ]

flights :: Database -> PassName -> [(FlightNumber,Airfare)]
flights db name = qtsort $ map select $ filter (getName name) db
    where
        getName :: PassName -> Entry -> Bool
        getName name (x,_,_,_,_) = x == name
        select :: (a,b,c,d,e) -> (b,e)
        select (_,a,_,_,b) = (a,b)
        
