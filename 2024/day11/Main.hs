{-# LANGUAGE TupleSections #-}

module Main where

import Prelude
import System.Directory.Internal.Prelude (getArgs)
import qualified Data.Map.Strict as Map

processFile :: FilePath -> IO [Int]
processFile filePath = do
    content <- readFile filePath
    let stones = map (\s -> read s :: Int) $ words content
    return stones

execN :: (a -> a) -> Int -> a -> a
execN _ 0 a = a
execN f n a = execN f (n-1) (f a)

evenDigitNumber :: Int -> Bool
evenDigitNumber i = even $ length $ show i

splitStone :: Int -> [Int]
splitStone stone =
    let asStr = show stone
        left = read (take (length asStr `div` 2) asStr) :: Int
        right = read (drop (length asStr `div` 2) asStr) :: Int
    in [left, right]

summarize :: [(Int, Int)] -> [(Int, Int)]
summarize [] = []
summarize xs = Map.toList $ foldl updateMapValue Map.empty xs

updateMapValue :: Map.Map Int Int -> (Int,Int) -> Map.Map Int Int
updateMapValue m (k, v) = Map.insert k updatedValue m
    where updatedValue = case Map.lookup k m of
            Just curr -> curr + v
            Nothing -> v

blink :: [(Int, Int)] -> [(Int, Int)]
blink stones = summarize $ blink' stones
    where
        blink' [] = []
        blink' ((0,a):ss) = (1,a): blink' ss
        blink' ((s,a):ss)
            | evenDigitNumber s = let [l,r] = splitStone s in (l,a):(r,a): blink' ss
            | not $ evenDigitNumber s = (s * 2024, a) : blink' ss

part01 :: [Int] -> Int
part01 stones = foldl (\acc (_, x) -> acc + x) 0  $ execN blink 25 $ map (,1) stones

part02 :: [Int] -> Int
part02 stones = foldl (\acc (_, x) -> acc + x) 0  $ execN blink 75 $ map (,1) stones

main :: IO ()
main = do
    args <- getArgs
    stones <- processFile $ head args
    print $ part01 stones
    print $ part02 stones
