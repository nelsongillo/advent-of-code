import System.IO
import Prelude
import Data.List
import Data.Foldable (foldr)
import System.Directory.Internal.Prelude (getArgs)

processFile :: FilePath -> IO [[Integer]]
processFile filePath = do
    content <- readFile filePath
    let report = map (map (\ v -> read v :: Integer) . words) (lines content)
    return report

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

adjDiff :: [Integer] -> [Integer]
adjDiff xs = zipWith (-) (tail xs) xs

sameSign :: [Integer] -> Bool
sameSign xs =
    let ss =  map (> 0) xs
    in all (== head ss) ss

inRange :: (Foldable t, Ord a) => a -> a -> t a -> Bool
inRange l h = all (\ v -> l <= v && v <= h)

reportSafe :: [Integer] -> Bool
reportSafe r =
    let diffs = adjDiff r
    in sameSign diffs && inRange 1 3 (map abs diffs)

reportOptions :: [a] -> [[a]]
reportOptions xs = [take i xs ++ drop (i + 1) xs | i <- [0 .. length xs - 1]]

dampenedReportSafe :: [Integer] -> Bool
dampenedReportSafe r = any reportSafe $ reportOptions r

part01 :: [[Integer]] -> Int
part01 r = count True $ map reportSafe r

part02 ::[[Integer]] -> Int
part02 r = count True $ map dampenedReportSafe r

main :: IO ()
main = do
    args <- getArgs
    input <- processFile $ head args
    print $ part01 input
    print $ part02 input
