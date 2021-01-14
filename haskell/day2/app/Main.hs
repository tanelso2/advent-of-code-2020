module Main where

import Data.Either
import Lib
import System.IO
import Text.ParserCombinators.Parsec

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    -- TODO: Don't ignore parse errors
    passwords <- return $ rights $ map (\x -> parse parseLine "" x) $ lines contents
    putStrLn $ "The answer to part 1 is " ++ (show . part1) passwords
    putStrLn $ "The answer to part 2 is " ++ (show . part2) passwords
    hClose handle

part1 :: [PasswordInput] -> Int
part1 p = length $ filter part1Validate p

part1Validate :: PasswordInput -> Bool
part1Validate (Password lowerBound upperBound targetChar password) =
    occurences >= lowerBound && occurences <= upperBound
      where occurences = length $ filter (\x -> x == targetChar) password

part2 :: [PasswordInput] -> Int
part2 p = length $ filter part2Validate p

part2Validate :: PasswordInput -> Bool
part2Validate (Password lowerBound upperBound targetChar password) =
    (lchar == targetChar) `xor` (uchar == targetChar)
        where lchar = password !! (lowerBound - 1)
              uchar = password !! (upperBound - 1)

xor :: Bool -> Bool -> Bool
xor True True = False
xor True False = True
xor False True = True
xor False False = False

-- Example Inputs:
-- 1-3 a: abcde
-- 1-3 b: cdefg
-- 2-9 c: ccccccccc

data PasswordInput = Password Int Int Char String
    deriving (Show)

parseLine :: Parser PasswordInput
parseLine = do
    lowerBoundStr <- many digit
    char '-'
    upperBoundStr <- many digit
    spaces
    targetLetter <- letter
    char ':'
    spaces
    password <- many letter
    lowerBound <- return $ parseInt lowerBoundStr
    upperBound <- return $ parseInt upperBoundStr
    return $ Password lowerBound upperBound targetLetter password

parseInt :: String -> Int
parseInt = read
