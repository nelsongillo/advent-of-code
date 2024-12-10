module Main where

import Prelude
import Data.List (nub)
import Data.Array (Ix, (!), array, bounds, Array)
import System.Directory.Internal.Prelude (getArgs)

processFile :: FilePath -> IO (Array Int (Array Int Char), [Char])
processFile filePath = do
    content <- readFile filePath
    let ls = lines content
        nodes = filter (\c -> c /='.' && c /= '\n') $ nub content
        roof = array (0,length ls - 1) $ zip [0..] $ map (\l -> array (0,length l - 1) $ zip [0..] l) ls
    return (roof, nodes)

height :: Array i e -> i
height m = let (_, h) = bounds m in h

width :: (Ix i1, Num i1) => Array i1 (Array i2 e) -> i2
width m = let (_, w) = bounds $ m ! 0 in w

inBounds :: (Eq a, Enum a, Num a, Enum i, Num i, Ix i) => Array i (Array a e) -> (a, i) -> Bool
inBounds area (x, y) = elem y [0..height area] && elem x [0..width area]

matrixIndices :: (Num a, Enum a, Num b, Enum b, Ix b) => Array b (Array a e) -> [(a, b)]
matrixIndices m = [(i, j) | i <- [0..width m], j <- [0..height m]]

distance :: (Int, Int) -> (Int, Int) -> (Int, Int)
distance (ax, ay) (bx, by) = (ax - bx, ay - by)

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (a, b) (c, d) = (a + c, b + d)

projectWhile :: ((Int, Int) -> Bool) -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
projectWhile p from vector = projectWhile' from []
    where projectWhile' f acc
            | p $ add f vector  = projectWhile' (add f vector) (add f vector:acc)
            | not $ p $ add f vector = acc
            | otherwise = error "Unexpected case"

nodeLocations :: Array Int (Array Int Char) -> Char -> [(Int, Int)]
nodeLocations roof node = filter (\(x, y) -> roof ! y ! x == node) $ matrixIndices roof

withoutIndex :: [a] -> Int -> [a]
withoutIndex xs i = take i xs ++ drop (i + 1) xs

limitedAntiNodes :: Array Int (Array Int Char) -> Char -> [(Int, Int)]
limitedAntiNodes roof node =
    let nodes = nodeLocations roof node
    in nub $ concatMap (\i -> limitedAntiNodes' (nodes !! i) (withoutIndex nodes i)) [0..(length nodes - 1)]
    where limitedAntiNodes' this other =
            filter (inBounds roof) $ map (add this . distance this) other

limitlessAntiNodes :: Array Int (Array Int Char) -> Char -> [(Int, Int)]
limitlessAntiNodes roof node =
    let nodes = nodeLocations roof node
    in nub $ nodes ++ concatMap (\i -> limitedAntiNodes' (nodes !! i) (withoutIndex nodes i)) [0..(length nodes - 1)]
    where limitedAntiNodes' this = 
            concatMap (projectWhile (inBounds roof) this . distance this)


part01 :: Array Int (Array Int Char) -> [Char] -> Int
part01 roof nodes = length $ nub $ concatMap (limitedAntiNodes roof) nodes

part02 :: Array Int (Array Int Char) -> [Char] -> Int
part02 roof nodes = length $ nub $ concatMap (limitlessAntiNodes roof) nodes

main :: IO ()
main = do
    args <- getArgs
    (roof, nodes) <- processFile $ head args
    print $ part01 roof nodes
    print $ part02 roof nodes