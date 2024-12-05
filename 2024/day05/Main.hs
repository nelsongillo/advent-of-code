module Main where

import System.IO
import Prelude
import Data.List (sortBy)
import Data.List.Split (splitOn)
import System.Directory.Internal.Prelude (getArgs)

processFile :: FilePath -> IO ([(Int, Int)],[[Int]])
processFile filePath = do
    content <- readFile filePath
    let parts = splitOn "\n\n" content
        order = map (toTuple . map (\v -> read v ::Int) . splitOn "|") $ lines $ head parts
        updates =  map (map (\v -> read v ::Int) . splitOn ",") $ lines $ parts !! 1
    return (order, updates)

contains :: (a -> Bool) -> [a] -> Bool
contains _ [] = False
contains predicate (x:xs) = predicate x || contains predicate xs

toTuple :: [a] -> (a, a)
toTuple [a,b] = (a,b)
toTuple xs = error $ "Unexpected list length" ++ show (length xs)

-- assuming list contains odd number of elements
middle :: [a] -> a
middle xs = xs !! (length xs `div` 2)

fixIncorrectUpdate :: [(Int, Int)] -> [Int] -> [Int]
fixIncorrectUpdate rules update =
    let relevantRules = filter (\(before, after) -> contains (==before) update && contains (==after) update) rules
    in sortBy (\a b -> if contains (\(ca, cb) -> ca == a && cb == b) relevantRules then GT else EG) update


isUpdateValid :: [(Int, Int)] -> [Int] -> Bool
isUpdateValid rules update =
    let prev idx = if idx == 0 then [] else take idx update
        relevantRules current = filter (\r -> current == snd r) rules
        needsToBeThere current = map fst $ relevantRules current
        isValid (idx, current) = all (\ p -> contains (== p) $ needsToBeThere current) (prev idx)
    in  all isValid (zip [0 .. ] update)

part01 :: [(Int, Int)] -> [[Int]] -> Int
part01 rules updates = sum $ map middle $ filter (isUpdateValid rules) updates

part02 :: [(Int, Int)] -> [[Int]] -> Int
part02 rules updates = sum $ map (middle . fixIncorrectUpdate rules) $ filter (not . isUpdateValid rules) updates

main :: IO ()
main = do
    args <- getArgs
    (rules, updates) <- processFile $ head args
    print $ part01 rules updates
    print $ part02 rules updates
