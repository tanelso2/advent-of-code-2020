module Main where

import Lib

import Data.Either
import System.IO
import Control.Monad.State

import Text.ParserCombinators.Parsec hiding (State)

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    -- TODO: Don't ignore parse errors
    ridges <- return $ rights $ map (\x -> parse parseLine "" x) $ lines contents
    -- putStrLn $ (show ridges)
    putStrLn $ "The answer to part one is " ++ (show (part1 ridges))
    putStrLn $ "The answer to part two is " ++ (show (part2 ridges))
    hClose handle

data SkiSlope = Tree | EmptySpace
  deriving (Show, Eq)

type Ridge = [[SkiSlope]]

part1 :: Ridge -> Int
part1 r = calculateCollisions r 3 1

part2 :: Ridge -> Int
part2 ridge = foldl (\acc (dx,dy) -> acc * (calculateCollisions ridge dx dy)) 1 testcases
    where testcases = [(1,1), (3,1), (5, 1), (7, 1), (1, 2)]

calculateCollisions :: Ridge -> Int -> Int -> Int
calculateCollisions ridge dx dy = length $ filteri hitTree ridge
    where
      hitTree y slopeList =  (slopeList !! idx) == Tree && visitedFloor
        where idx = (dx * y) `mod` (length slopeList)
              visitedFloor = (0 == ((y+1) `mod` dy))

mapi :: ((Int, a) -> b) -> [a] -> [b]
mapi f l = map f $ indexed l

filteri :: (Int -> a -> Bool) -> [a] -> [a]
filteri f l = map (\(_, x) -> x) $ filter (\(i, x) -> f i x) $ indexed l

indexed :: [a] -> [(Int, a)]
indexed x = zip indexes vals
    where (indexes, vals) = indexed_ x

indexed_ :: [a] -> ([Int], [a])
indexed_ [] = ([], [])
indexed_ (x:xs) = (indexes ++ [length indexes], x:vals)
    where (indexes, vals) = indexed_ xs



-- -- How do I use the State Monad to keep track of index while iterating over list?
-- bar :: State [(a, Int)] [(a, Int)]
-- bar = do
--    x <- get
--    put $ x ++ [length x]
--    return $ x
--
-- -- foo = foldl (\state x -> runState bar [x]) [1..10]
--
-- baz :: Int -> State [(a, Int)] [a]
-- baz n = sequence (take n (repeat bar))


parseLine :: Parser [SkiSlope]
parseLine = do
    many parseSpace

parseSpace :: Parser SkiSlope
parseSpace = do
    x <- oneOf ".#"
    return $ case x of
        '.' -> EmptySpace
        '#' -> Tree


