module Main where

import System.IO
import Prelude
import Data.List (isPrefixOf)
import System.Directory.Internal.Prelude (getArgs)
import Text.Regex.TDFA


processFilePart01 :: FilePath -> IO [String]
processFilePart01 filePath = do
    content <- readFile filePath
    let ops = extractOps content
    return ops


opsRegex :: String
opsRegex = "do\\(\\)|don't\\(\\)|mul\\([0-9]{1,3},[0-9]{1,3}\\)"

valueRegex :: String
valueRegex = "[0-9]{1,3}"

extractOps :: String -> [String]
extractOps input = getAllTextMatches (input =~ opsRegex) :: [String]

filterOpsUnconditional :: [String] -> [String]
filterOpsUnconditional = filter (isPrefixOf "mul")

filterOpsConditional :: [String] -> [String]
filterOpsConditional input =
    let (_, result) = foldl helper (True, []) input
    in result
    where 
        helper :: (Bool, [String]) -> String -> (Bool, [String])
        helper (_, ops)     "don't()"   = (False, ops)
        helper (_, ops)     "do()"      = (True, ops)
        helper (True, ops)  ('m':xs)    = (True, xs:ops)
        helper (False, ops) ('m':_)     = (False, ops)
        helper _            _           = error "Unexpected pattern" 


parseMultiplications :: [String] -> [(Integer, Integer)]
parseMultiplications muls =
    let rawValues = map (\operation -> getAllTextMatches (operation =~ valueRegex) :: [String]) muls
        intValues = map (\v -> read v :: Integer) $ concat rawValues
    in pairUp intValues

pairUp :: [a] -> [(a, a)]
pairUp [] = []
pairUp (x:y:xs) = (x, y) : pairUp xs
pairUp _ = error "List has an odd number of elements"

part01 :: [String] -> Integer
part01 input = sum $ map (uncurry (*)) $ parseMultiplications $ filterOpsUnconditional input

part02 :: [String] -> Integer
part02 input = sum $ map (uncurry (*)) $ parseMultiplications $ filterOpsConditional input

main :: IO ()
main = do
    args <- getArgs
    input <- processFilePart01 $ head args
    print $ part01 input
    print $ part02 input
