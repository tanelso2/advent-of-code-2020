module Main where

import Lib
import System.IO

import qualified Data.Map.Strict as M
import Text.ParserCombinators.Parsec


main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    -- TODO: Don't ignore parse errors
    -- ridges <- return $ rights $ map (\x -> parse parseLine "" x) $ lines contents
    -- putStrLn $ (show ridges)
    -- putStrLn $ show $ parse parsePassports "" contents
    putStrLn $ show $  (\(Right x) -> x) $ parse parsePassportsString "" contents
    putStrLn $ show $ length $ (\(Right x) -> x) $ parse parsePassportsString "" contents
    -- putStrLn $ show $ lines contents
    -- putStrLn $ show $ take 75 (reverse contents)
    -- putStrLn $ "The answer to part one is " ++ (show (part1 ridges))
    -- putStrLn $ "The answer to part two is " ++ (show (part2 ridges))
    hClose handle

-- type Passport = M.Map String String
type Passport = [(String, String)]

parsePassports :: Parser [Passport]
parsePassports = do
    x <- many1 parsePassport
    return x

passportSeparator = emptyLine

parsePassport :: Parser Passport
parsePassport = do
    x <- sepBy1 parseField fieldSeparator
    char '\n'
    return $ x

parsePassportsString :: Parser Passport
parsePassportsString = do
    sepBy1 parseField ((string "\n\n") <|> string " " <|> string "\n")

-- parsePassportString :: Parser String
-- parsePassportString = do
--     y <- sepBy1 (many anyChar) emptyLine
--     -- x <- many anyChar
--     -- emptyLine <|> eof
--     return y

fieldSeparator = newline <|> space

emptyLine :: Parser String
emptyLine = string "\n\n"

parseField :: Parser (String, String)
parseField = do
    x <- many1 $ oneOf ['a'..'z']
    char ':'
    y <- many1 $ oneOf (['a'..'z']++['0'..'9']++['#'])
    return $ (x, y)
