module Nonogram
	( Grid(..)
	, getCols
	, renderGrid
	, parseGrid
	, Hints(..)
	)
where

import Data.List (transpose)

newtype Grid x =
	Grid { getRows :: [[x]] }
	deriving (Eq, Ord)

getCols :: Grid x -> [[x]]
getCols = transpose . getRows

renderGrid :: Show x => Grid x -> String
renderGrid = show . getRows

parseGrid :: Read x => String -> Grid x
parseGrid = Grid . read

data Hints =
	Hints { rowHints :: [[Int]], colHints :: [[Int]] }
	deriving (Eq, Ord, Show, Read)