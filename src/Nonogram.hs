module Nonogram
	( Row(..)
	, Nonogram(..)
	, getVals
	, fromVals
	, renderNonogram
	, parseNonogram
	, Hints(..)
	)
where

newtype Row x =
	Row { fromRow :: [x] }
	deriving (Eq, Ord)

newtype Nonogram x =
	Nonogram { getRows :: [Row x] }
	deriving (Eq, Ord)

getVals :: Nonogram x -> [[x]]
getVals = fmap fromRow . getRows

fromVals :: [[x]] -> Nonogram x
fromVals = Nonogram . fmap Row

renderNonogram :: Show x => Nonogram x -> String
renderNonogram = show . getVals

parseNonogram :: Read x => String -> Nonogram x
parseNonogram = fromVals . read

data Hints =
	Hints { rows :: [[Int]], cols :: [[Int]] }
	deriving (Eq, Ord, Show, Read)