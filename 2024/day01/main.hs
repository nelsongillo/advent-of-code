import System.IO
import Prelude
import Data.List
import Data.Maybe (catMaybes)
import System.Directory.Internal.Prelude (getArgs)

processFile :: FilePath -> IO ([String], [String])
processFile filePath = do
    content <- readFile filePath
    let (left, right) = unzip $ map splitLine (lines content)
    return (left, right)

splitLine :: String -> (String, String)
splitLine line = case words line of
    (x:y:_) -> (x, y)
    _        -> ("", "")

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

part01 :: [Int] -> [Int] -> Int
part01 l r = sum $ map abs $ zipWith (-) l r

part02 :: [Int] -> [Int] -> Int
part02 l r = sum $ map (\v -> v * count v r)  l


main :: IO ()
main = do
    args <- getArgs
    (l, r) <- processFile $ head args
    let left = sort $ map (\v -> read v :: Int) l
        right = sort $ map (\v -> read v :: Int) r
    print $ part01 left right
    print $ part02 left right
