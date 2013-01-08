type InitialValue = Integer
type NumberOfRounds = Integer
type MaxRounds = Integer
type FinalValue = Integer
type TextRep = String
data Solution = Failure | Success (NumberOfRounds, FinalValue, TextRep)
    deriving (Eq, Show)


isPalindrome :: Integer -> Bool
isPalindrome n = if k == reverse k then True else False
    where k = show . abs $ n

rev :: Integer -> Integer
rev n = read $ reverse . show $ n :: Integer
-- :: Integer nur wichtig wenn keine Typsignatur verwendet wird

addRev :: InitialValue -> MaxRounds -> Solution
addRev init max = add (abs init) (abs max) 0
    where
        add :: InitialValue -> MaxRounds -> NumberOfRounds -> Solution
        add n max rounds | isPalindrome n = Success (rounds, n, show n)
                         | rounds == max = Failure
                         | otherwise = add (n + (rev n)) max (rounds + 1)


type Country = String
type Countries = [Country]
type TravelTime = Integer -- Travel time in minutes
data Connection = Air Country Country TravelTime
                | Sea Country Country TravelTime
                | Rail Country Country TravelTime
                | Road Country Country TravelTime deriving (Eq,Ord,Show)
type Connections = [Connection]
data Itinerary = NoRoute | Route (Connections,TravelTime) deriving (Eq,Ord,Show)
