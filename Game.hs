module Game
( ListOfValues
, TargetValue
, Game
, Operators(Plus,Times,Minus,Div)
, Solution
) where

type ListOfValues = [Integer]
type TargetValue  = Integer
type Game         = (ListOfValues,TargetValue)
data Operators    = Plus | Times | Minus | Div deriving (Eq,Ord,Show)
type Solution     = [Operators]
