module Main where

import System.IO
import Prelude
import Data.Array
import Data.List (nub)
import System.Directory.Internal.Prelude (getArgs)

data Direction = North | East | South | West deriving (Eq, Enum)

processFile :: FilePath -> IO (Array Int (Array Int Char))
processFile filePath = do
    content <- readFile filePath
    let ls = lines content
        area = array (0,length ls - 1) $ zip [0..] $ map (\l -> array (0,length l - 1) $ zip [0..] l) ls
    return area

height :: Array i e -> i
height m = let (_, h) = bounds m in h

width :: (Ix i1, Num i1) => Array i1 (Array i2 e) -> i2
width m = let (_, w) = bounds $ m ! 0 in w

inBounds :: (Eq a, Enum a, Num a, Enum i, Num i, Ix i) => Array i (Array a e) -> (a, i) -> Bool
inBounds area (x, y) = elem y [0..height area] && elem x [0..width area]

setObstruction :: (Ix i1, Ix i2) => Array i1 (Array i2 Char) -> (i2, i1) -> Array i1 (Array i2 Char)
setObstruction area (x, y) = area // [(y, area ! y // [(x, '#')])]

incCoords :: (Int, Int) -> Direction -> (Int, Int)
incCoords (x, y) North = (x, y - 1)
incCoords (x, y) East = (x + 1, y)
incCoords (x, y) South = (x, y + 1)
incCoords (x, y) West = (x - 1, y)

findStart :: Array Int (Array Int Char) -> (Int, Int)
findStart area = findStart' area 0 0
    where findStart' a x y
            | a ! y ! x == '^' = (x,y)
            | x < width a = findStart' a (x + 1) y
            | x == width a && y < height a = findStart' a 0 (y + 1)
            | otherwise = error $ "Unexpected index (" ++ show x ++ ", " ++ show y ++ ")"
        
canWalk :: Array Int (Array Int Char) -> (Int, Int) -> Direction -> Bool
canWalk area (x, y) d =
    let (xx, yy) = incCoords (x, y) d
    in inBounds area (xx, yy) && area ! yy ! xx /= '#'

walk :: Array Int (Array Int Char) -> [(Int, Int)]
walk area = nub $ walk' area (findStart area) North [findStart area]
    where walk' a (x, y) direction path
            | canWalk a (x, y) direction = walk' a (incCoords (x, y) direction) direction $ (x,y):path
            | inBounds a $ incCoords (x, y) direction = walk' a (x, y) (toEnum $ mod (fromEnum direction + 1) 4) path
            | otherwise = (x,y):path

-- if guard has done more steps than the area is big -> loop
detectLoop :: Array Int (Array Int Char) -> Bool
detectLoop area =
    let (x, y) = findStart area
    in detectLoop' area (x, y) North 0
    where detectLoop' a (x, y) direction count
            | count > width area * height area = True
            | canWalk a (x, y) direction = detectLoop' a (incCoords (x, y) direction) direction $ count + 1
            | inBounds a $ incCoords (x, y) direction = detectLoop' a (x, y) (toEnum $ mod (fromEnum direction + 1) 4) count
            | otherwise = False

part01 :: Array Int (Array Int Char) -> Int
part01 area = length $ walk area

part02 :: Array Int (Array Int Char) -> Int
part02 area = 
    let start = findStart area
    in length $ filter id $ map (\(x, y) -> detectLoop $ if start /= (x, y) then setObstruction area (x, y) else area) $ walk area


main :: IO ()
main = do
    args <- getArgs
    area <- processFile $ head args
    print $ part01 area
    print $ part02 area
