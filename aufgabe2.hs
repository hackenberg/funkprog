kgv :: Integer -> Integer -> Integer
--kgv _ 0 = 
--kgv 0 _ = 
kgv a b = lcm a b

agv :: Integer -> Integer -> (Integer,Integer) -> [Integer]
agv 0 _ _ = []
agv _ 0 _ = []
agv a b (x,y) = takeWhile (>=x) $ takeWhile (<=y) $ map ((kgv a b)*) [1..]
