module Main where

import Prelude
import Data.List (nub)
import Data.Char (digitToInt)
import Data.Array (Ix, (!), array, bounds, Array)
import System.Directory.Internal.Prelude (getArgs)

processFile :: FilePath -> IO (Array Int (Array Int Int))
processFile filePath = do
    content <- readFile filePath
    let ls = lines content
        area = array (0,length ls - 1) $ zip [0..] $ map (\l -> array (0,length l - 1) $ zip [0..] (map digitToInt l))  ls
    return area

height :: Array i e -> i
height m = let (_, h) = bounds m in h

width :: (Ix i1, Num i1) => Array i1 (Array i2 e) -> i2
width m = let (_, w) = bounds $ m ! 0 in w

inBounds :: (Eq a, Enum a, Num a, Enum i, Num i, Ix i) => Array i (Array a e) -> (a, i) -> Bool
inBounds area (x, y) = elem y [0..height area] && elem x [0..width area]

getNeighbors :: (Int, Int) -> [(Int, Int)]
getNeighbors (x, y) = [(x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)]

matrixIndices :: (Num a, Enum a, Num b, Enum b, Ix b) => Array b (Array a e) -> [(a, b)]
matrixIndices m = [(i, j) | i <- [0..width m], j <- [0..height m]]

walkTrail :: Array Int (Array Int Int) -> (Int, Int) -> [(Int, Int)]
walkTrail area (x,y) = walkTrail' area 0 [(x,y)]
    where walkTrail' a c potentials
            | null potentials = []
            | c == 9 = potentials
            | otherwise = walkTrail' a (c+1) $ concatMap (filter (\ (nx, ny) -> inBounds a (nx, ny) && a ! ny ! nx == c + 1) . getNeighbors) potentials

scoreTrails :: Array Int (Array Int Int) -> [Int]
scoreTrails area = map (length . nub . walkTrail area) $ filter (\(x,y) -> area ! y ! x == 0) $ matrixIndices area

rateTrails :: Array Int (Array Int Int) -> [Int]
rateTrails area = map (length . walkTrail area) $ filter (\(x,y) -> area ! y ! x == 0) $ matrixIndices area

part01 :: Array Int (Array Int Int) -> Int
part01 area = sum $ scoreTrails area

part02 :: Array Int (Array Int Int) -> Int
part02 area = sum $ rateTrails area

main :: IO ()
main = do
    args <- getArgs
    area <- processFile $ head args
    print $ part01 area
    print $ part02 area
