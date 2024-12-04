module Main where

import System.IO
import Prelude
import Data.List (transpose)
import System.Directory.Internal.Prelude (getArgs)
import Text.Regex.TDFA ( (=~), AllTextMatches(getAllTextMatches) )

data Orientation = LeftRight
                | RightLeft
                | Topdown
                | Bottomup
                | DiagLeftRight
                | DiagLeftRightReverse
                | DiagRightLeft
                | DiagRightLeftReverse
    deriving Enum

processFile :: FilePath -> IO [String]
processFile filePath = do
    content <- readFile filePath
    let l = lines content
    return l


xmasRegex :: String
xmasRegex = "XMAS"

findXmas :: String ->  Int
findXmas input = length (getAllTextMatches (input =~ xmasRegex) :: [String])

assembleLines ::  [String] -> Orientation -> [String]
assembleLines l LeftRight = l
assembleLines l RightLeft =  map reverse l
assembleLines l Topdown =  transpose l
assembleLines l Bottomup = map reverse $ transpose l
assembleLines l DiagLeftRight = diagonals l
assembleLines l DiagLeftRightReverse = diagonals $ rotate180 l
assembleLines l DiagRightLeft =  diagonals $ rotate180 $ reverse l
assembleLines l DiagRightLeftReverse =  diagonals $ reverse l

rotate180 :: [[a]] -> [[a]]
rotate180 = map reverse . reverse

diagonals :: [[a]] -> [[a]]
diagonals = (++) <$> transpose . zipWith drop [0..]
                 <*> map reverse . transpose . zipWith drop [1..] . rotate180

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

matrixIndices :: [[a]] -> [(Int, Int)]
matrixIndices matrix =
    let numRows = length matrix
        numCols = if numRows > 0 then length (head matrix) else 0
    in [(row, col) | row <- [0..numRows-1], col <- [0..numCols-1]]

diagonalNeighbors ::  [String] -> Int -> Int -> [Char]
diagonalNeighbors matrix row col = 
    let numRows = length matrix
        numCols = if numRows > 0 then length (head matrix) else 0
        isValid (r, c) = r >= 0 && c >= 0 && r < numRows && c < numCols
        offsets = [(-1, -1), (-1, 1), (1, -1), (1, 1)]
        neighbors = [(row + dr, col + dc) | (dr, dc) <- offsets, isValid (row + dr, col + dc)]
    in [matrix !! r !! c | (r, c) <- neighbors]

checkForCross :: [String] -> Int -> Int -> Bool
checkForCross matrix x y =
    let ns = diagonalNeighbors matrix x y 
    in ns == "MSMS" || ns == "SMSM" || ns == "SSMM" || ns == "MMSS"

checkIndex :: [String] -> (Int, Int) -> Bool
checkIndex matrix (x,y) =
    let needCheck = matrix !! x !! y == 'A'
    in needCheck && checkForCross matrix x y

part01 :: [String] -> Int
part01 input = sum $ concatMap (map findXmas . assembleLines input) (enumFrom (toEnum 0) :: [Orientation])

part02 :: [String] -> Int
part02 input = count True $ map (checkIndex input) $ matrixIndices input

main :: IO ()
main = do
    args <- getArgs
    input <- processFile $ head args
    print $ part01 input
    print $ part02 input
