-- Given a nonogram on each line, output its hints on each line

import Data.List (group, transpose)
import Nonogram (Nonogram(..), getVals, parseNonogram, Hints(..))

hintRow :: [Bool] -> [Int]
hintRow = fmap length . filter head . group

hintRows :: [[Bool]] -> [[Int]]
hintRows = fmap hintRow

hintNonogram :: Nonogram Bool -> Hints
hintNonogram nonogram = Hints { rows, cols } where
	listrep = getVals nonogram
	rows = hintRows listrep
	cols = hintRows $ transpose listrep

main :: IO ()
main = interact $ unlines . fmap (show . hintNonogram . parseNonogram) . lines