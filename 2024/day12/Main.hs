module Main where

import Prelude
import Data.List (sort, sortBy, groupBy)
import Data.Function (on)
import Data.Array (Ix, (!), (//), array, bounds, Array)
import System.Directory.Internal.Prelude (getArgs, catMaybes)

-- Matrix Helper functions
type Point = (Int, Int)

data Direction = North | East | South | West deriving (Show, Eq, Enum)

getDirection :: Point -> Point -> Direction
getDirection (ax, ay) (bx, by)
    | ax == bx && by > ay = South
    | ax == bx && by < ay = North
    | ay == by && bx > ax = East
    | ay == by && bx < ax = West
    | otherwise = error "unexpected direction"

height :: Array i e -> i
height m = let (_, h) = bounds m in h

width :: (Ix i1, Num i1) => Array i1 (Array i2 e) -> i2
width m = let (_, w) = bounds $ m ! 0 in w

inBounds :: (Eq a, Enum a, Num a, Enum i, Num i, Ix i) => Array i (Array a e) -> (a, i) -> Bool
inBounds m (x, y) = elem y [0..height m] && elem x [0..width m]

getNeighbors :: Point -> [Point]
getNeighbors (x, y) = [(x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)]

matrixIndices :: (Num a, Enum a, Num b, Enum b, Ix b) => Array b (Array a e) -> [(a, b)]
matrixIndices m = [(i, j) | i <- [0..width m], j <- [0..height m]]

-- Code for the actual problem
data Region = Region Char [Point] [(Direction, Point)]
    deriving (Show)

data Plot = Plot Char Bool
    deriving (Show)

initPlot :: Char -> Plot
initPlot c = Plot c False

setVisited :: (Ix a, Ix i) => Array i (Array a Plot) -> (a, i) -> Array i (Array a Plot)
setVisited garden (x, y) = let (Plot p _) = garden ! y ! x in garden // [(y, garden ! y // [(x, Plot p True)])]

isVisited :: (Ix a, Ix b) => Array b (Array a Plot) -> (a, b) -> Bool
isVisited garden (x, y) = let (Plot _ v) = garden ! y ! x in v

sameRegion :: Array Int (Array Int Plot) -> Point -> Point -> Bool
sameRegion garden (ax, ay) (bx, by) =
    let (Plot at _) = garden ! ay ! ax
        (Plot bt _) = garden ! by ! bx
    in bt == at

addPerimeter :: Region -> Point -> Point -> Region
addPerimeter (Region c a p) prev current  = Region c a ((getDirection prev current, current):p)

addArea :: Region -> Point -> Region
addArea (Region c a p) i = Region c (i:a) p

searchRegion :: Array Int (Array Int Plot) -> Region -> Point -> (Array Int (Array Int Plot), Region)
searchRegion garden region start = foldl (\(g, r) next -> searchRegion' g r start next) (setVisited garden start, addArea region start) $ getNeighbors start
    where searchRegion' g r prev current@(x, y)
            | not $ inBounds g (x, y) = (g, addPerimeter r prev current)
            | not $ sameRegion g prev current = (g, addPerimeter r prev current)
            | not $ isVisited g current = foldl (\(gg, rr) next -> searchRegion' gg rr current next) (setVisited g current, addArea r current) $ getNeighbors current
            | otherwise = (g, r)

extractRegions :: Array Int (Array Int Plot) -> [Region]
extractRegions garden = catMaybes $ snd $ foldl (\(g, rs) i -> let (gg, r) = extractRegions' g i in (gg, r:rs)) (garden, []) $ matrixIndices garden
    where extractRegions' g i@(x,y) =
            let (Plot target visited) = g ! y ! x
            in if visited then (g, Nothing) else
                    let (gg, region) = searchRegion g (Region target [] []) i
                    in (gg, Just region)

groupByNeighbors :: (a -> a -> Bool) -> [a] -> [[a]]
groupByNeighbors _ [] = []
groupByNeighbors _ [x] = [[x]]
groupByNeighbors f (x:xs) = gbn [x] xs
  where gbn current [] = [current]
        gbn current (y:ys)
            | f (last current) y = gbn (current ++ [y]) ys
            | otherwise = current : gbn [y] ys

sortDirectionAware :: [(Direction, Point)] -> [Point]
sortDirectionAware [] = []
sortDirectionAware xs@((d, _):_)
    | d `elem` [East, West] = map (\(b, a) -> (a, b)) $ sort $ map snd xs
    | otherwise = map (\(b, a) -> (a, b)) $ sort $ map ((\(b, a) -> (a, b)) . snd) xs

countSides :: Region -> Int
countSides (Region _ area ps)
    | length area == 1 = 4
    | length area == 2 = 4
    | otherwise = sum $ map ((length . groupByNeighbors (\(ax, ay) (bx, by) -> ay == by && bx - ax == 1)) . sortDirectionAware) $ groupBy (\ a b -> fst a == fst b) $ sortBy (flip compare `on` (fromEnum .fst)) ps

part01 :: Array Int (Array Int Plot) -> Int
part01 garden = sum $ map (\(Region _ a p) -> length a * length p) $ extractRegions garden

part02 :: Array Int (Array Int Plot) -> Int
part02 garden = sum $ map (\r@(Region _ a _) -> length a * countSides r) $ extractRegions garden

processFile :: FilePath -> IO (Array Int (Array Int Plot))
processFile filePath = do
    content <- readFile filePath
    let ls = lines content
        garden = array (0,length ls - 1) $ zip [0..] $ map (\l -> array (0,length l - 1) $ zip [0..] $ map initPlot l)  ls
    return garden

main :: IO ()
main = do
    args <- getArgs
    garden <- processFile $ head args
    print $ part01 garden
    print $ part02 garden
