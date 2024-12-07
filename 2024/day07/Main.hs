module Main where

import System.IO
import Prelude
import Control.Monad (replicateM)
import Data.List.Split (splitOn)
import System.Directory.Internal.Prelude (getArgs)

data Op = Sum | Mul | Concat deriving (Show, Enum)

data Equation = Equation Int [Int]
    deriving (Show)

processFile :: FilePath -> IO [Equation]
processFile filePath = do
    content <- readFile filePath
    return $ map (\l -> let e = splitOn ":" l in Equation (read (head e) :: Int) (map (\v -> read v :: Int) $ words (e !! 1))) $ lines content

equationPossible :: [Op] -> Equation -> Bool
equationPossible opsPool (Equation result components)  =
    any (\ops -> result == eval components ops) $ replicateM (length components - 1) opsPool

eval :: [Int] -> [Op] -> Int
eval [x] [] = x
eval (a:b:xs) (Sum:ops) = eval ((a+b):xs) ops
eval (a:b:xs) (Mul:ops) = eval ((a*b):xs) ops
eval (a:b:xs) (Concat:ops) = eval ((read (show a ++ show b) :: Int):xs) ops
eval _ _ = error "unexpected case"


part01 :: [Equation] -> Int
part01 es = sum $ map (\(Equation r _) -> r) $ filter (equationPossible [Sum, Mul]) es

part02 :: [Equation] -> Int
part02 es = sum $ map (\(Equation r _) -> r) $ filter (equationPossible [Sum, Mul, Concat]) es

main :: IO ()
main = do
    args <- getArgs
    equations <- processFile $ head args
    print $ part01 equations
    print $ part02 equations