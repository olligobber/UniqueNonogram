module Nonogram
	( Grid(..)
	, getCols
	, storeGrid
	, parseGrid
	, Hints(..)
	, storeHints
	, parseHints
	)
where

import Data.List (transpose, intercalate)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn delim l = fst $ go (delim:l) where
	go [] = ([], [])
	go (x:xs)
		| x == delim = (uncurry (flip (:)) $ go xs, [])
		| otherwise = (x:) <$> go xs

newtype Grid x =
	Grid { getRows :: [[x]] }
	deriving (Eq, Ord)

getCols :: Grid x -> [[x]]
getCols = transpose . getRows

-- Store in a compact format
storeGrid :: Grid Bool -> String
storeGrid =
	intercalate "2" .
	fmap (fmap $ \b -> if b then '1' else '0') .
	getRows

-- Read the stored grid
parseGrid :: String -> Grid Bool
parseGrid =
	Grid .
	fmap (fmap (=='1')) .
	splitOn '2'

data Hints =
	Hints { rowHints :: [[Int]], colHints :: [[Int]] }
	deriving (Eq, Ord)

-- Store in a compact format
storeHints :: Hints -> String
storeHints hints = store rowHints <> "/" <> store colHints where
	store f = intercalate ";" $ fmap (intercalate "," . fmap show) $ f hints

-- Read the stored hints
parseHints :: String -> Hints
parseHints string = Hints {rowHints, colHints} where
	[rowString, colString] = splitOn '/' string
	rowHints = parseOne rowString
	colHints = parseOne colString
	parseOne = fmap (fmap read . splitOn ',') . splitOn ';'