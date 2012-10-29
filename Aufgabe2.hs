-- Teil 1
kgv :: Integer -> Integer -> Integer
kgv = lcm

-- Teil 2
agv :: Integer -> Integer -> (Integer,Integer) -> [Integer]
agv 0 _ _ = []
agv _ 0 _ = []
agv a b (x,y) = takeWhile (>=x) $ takeWhile (<=y) $ map ((kgv a b)*) [1..]

-- Teil 3
type PassName = String
type FlightNumber = Integer
type PlaceOfDeparture = String
type Destination = String
type Airfare = Integer
type Entry = (PassName,FlightNumber,PlaceOfDeparture,Destination,Airfare)
type Database = [Entry]

db =  [ ("Anton",857,"Vienna","London",237),
        ("Berta",456,"Paris","Berlin",278),
        ("Anton",123,"Rome","London",417),
        ("Anton",109,"London","Berlin",237),
        ("Karla",888,"Vienna","Rome",350),
        ("Karla",832,"Rome","London",417),
        ("Berta",857,"Vienna","London",199),
        ("Karla",753,"Vienna","London",237) ]

qtsort :: Ord a => [(a,b)] -> [(a,b)]
qtsort [] = []
qtsort (x:xs) = qtsort lesser ++ [x] ++ qtsort greater
    where
        lesser = [ a | a <- xs, (fst a) <= (fst x) ]
        greater = [ a | a <- xs, (fst a) > (fst x) ]

flights :: Database -> PassName -> [(FlightNumber,Airfare)]
flights db name = qtsort $ map select $ filter (getName name) db
    where
        getName :: PassName -> Entry -> Bool
        getName name (x,_,_,_,_) = x == name
        select :: Entry -> (FlightNumber,Airfare)
        select (_,x,_,_,y) = (x,y)

-- Teil 4
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort lesser ++ [x] ++ qsort greater
    where
        lesser = [ a | a <- xs, a <= x ]
        greater = [ a | a <- xs, a > x ]

pass2Dest :: Database -> Destination -> [PassName]
pass2Dest db dest = qsort $ map select $ filter (getDest dest) db
    where
        getDest :: Destination -> Entry -> Bool
        getDest dest (_,_,_,x,_) = x == dest
        select :: Entry -> PassName
        select (x,_,_,_,_) = x

-- Teil 5
--mostValuedPass :: Database -> PlaceOfDeparture -> Destination -> ([PassName],Airfare)
equals [] [] = True
equals [] _ = False
equals _ [] = False
equals (x:xs) (y:ys)
	| x == y = equals xs ys
	| otherwise = False

mostValuedPass db dep dest = filter (fifth == filter (equals dest . fourth) $ filter (equals dep . third) db
	where
		third (_,_,x,_,_) = x
		fourth (_,_,_,x,_) = x
