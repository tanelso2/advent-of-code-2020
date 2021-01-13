module Main where

import Lib
import System.IO

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    numbers <- return $ map parseInt $ lines contents
    part1 numbers
    part2 numbers
    hClose handle

parseInt :: String -> Int
parseInt = read

part1 :: [Int] -> IO ()
part1 x = putStrLn $ "The answer to part1 is " ++ (show . part1_) x

part1_ :: [Int] -> Int
part1_ xs = x * y
    where combinations = mapM (const xs) [1..2]
          filterFunc [a,b] = 2020 == a + b
          [x,y] = head $ filter filterFunc combinations

part2 :: [Int] -> IO ()
part2 x = putStrLn $ "The answer to part2 is " ++ (show . part2_) x

part2_ :: [Int] -> Int
part2_ xs = foldl1 (*) result
    where combinations = mapM (const xs) [1..3]
          filterFunc = (==2020) . sum
          result = head $ filter filterFunc combinations
