-- Take in the size and output all possible nonograms

import Control.Monad (replicateM)
import Nonogram (Row(..), Nonogram(..), renderNonogram)

allBool :: [Bool]
allBool = [False, True]

allRow :: Int -> [Row Bool]
allRow size = Row <$> replicateM size allBool

allNonogram :: Int -> [Nonogram Bool]
allNonogram size = Nonogram <$> replicateM size (allRow size)

main :: IO ()
main = do
	size <- readLn
	mapM_ (putStrLn . renderNonogram) $ allNonogram size