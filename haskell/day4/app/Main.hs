module Main where

import Lib
import System.IO
import Data.Either
import Data.List

import qualified Data.Map.Strict as M
import Text.ParserCombinators.Parsec

singleton :: a -> [a]
singleton x = [x]

handleSuccess :: [Passport] -> IO ()
handleSuccess passports = do
    putStrLn $ show passports
    putStrLn "Holy fuck it worked?"
    putStrLn "Number of passports parsed: "
    printAny $ length passports
    putStrLn "The answer to part1 is "
    printAny $ length $ filter part1IsValid passports
    putStrLn "The answer to part2 is "
    printAny $ length $ filter part2IsValid passports
    -- putStrLn "Part 2 failures"
    -- failures <- return $ filter (not . part2IsValid) passports
    -- putStrLn $ intercalate "\n~~~~~~~~~~~\n" (map failuresString failures)

failuresString :: Passport -> String
failuresString p = (show p) ++ "\n\nErrors: " ++ show failures 
    where failures = part2GetFailures p


handleResult :: Either ParseError [Passport] -> IO ()
handleResult = either handleError handleSuccess

printAny :: (Show a) => a -> IO ()
printAny = putStrLn . show

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    parseResult <- return $ parse parsePassports "" contents
    -- putStrLn $ show $ lines contents
    -- putStrLn $ show $ take 75 (reverse contents)
    -- putStrLn $ "The answer to part one is " ++ (show (part1 ridges))
    -- putStrLn $ "The answer to part two is " ++ (show (part2 ridges))
    handleResult parseResult
    hClose handle

type Passport = M.Map String String
-- type Passport = [(String, String)]

handleError :: ParseError -> IO ()
handleError err = do
    putStrLn "PARSE ERROR"
    putStrLn $ show err

parsePassports :: Parser [Passport]
parsePassports = do
    x <- sepEndBy1 parsePassport newline
    return x

parsePassport :: Parser Passport
parsePassport = do
    x <- sepEndBy1 parseField fieldSeparator
    return $ M.fromList x

fieldSeparator = newline <|> space

parseField :: Parser (String, String)
parseField = do
    x <- many1 $ oneOf ['a'..'z']
    char ':'
    y <- many1 $ oneOf (['a'..'z']++['0'..'9']++['#'])
    return $ (x, y)

requiredFields = ["byr", "pid", "iyr", "eyr", "hgt", "hcl", "ecl"]

part1IsValid :: Passport -> Bool
part1IsValid p = all (\x -> M.member x p) requiredFields

part2IsValid :: Passport -> Bool
part2IsValid p = (part1IsValid p) && all (validateField p) requiredFields

part2GetFailures :: Passport -> [String]
part2GetFailures p = filter (\x -> not (validateField p x)) requiredFields

validateField :: Passport -> String -> Bool
validateField p field =
    case M.lookup field p of
        Nothing -> False
        Just x ->
            case field of
                "byr" -> parseAndValidate (parseIntFromDigits 4) (inBetween 1920 2002) x
                "iyr" -> parseAndValidate (parseIntFromDigits 4) (inBetween 2010 2020) x
                "eyr" -> parseAndValidate (parseIntFromDigits 4) (inBetween 2020 2030) x
                "hgt" -> parseAndValidate parseHeight validHeight x
                "hcl" -> parseAndValidate parseHairColor (\x -> length x == 6) x 
                "ecl" -> parseAndValidate parseEyeColor (\_ -> True) x
                "pid" -> parseAndValidate (parseIntFromDigits 9) (\_ -> True) x
                _ -> True -- idgaf about any other fields that may or may not exist"

parseAndValidate :: Parser a -> (a -> Bool) -> String -> Bool
parseAndValidate parser pred s = either (\_ -> False) pred $ parse parser "" s

inBetween :: Int -> Int -> Int -> Bool
inBetween l h x = l <= x && x <= h

parseEyeColor :: Parser String
parseEyeColor = choice $ map (try . string) ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

parseHairColor :: Parser String
parseHairColor = do
    char '#'
    x <- many1 hexDigit
    return x

validHeight :: Height -> Bool
validHeight (Cm x) = inBetween 150 193 x
validHeight (Inch x) = inBetween 59 76 x

data Height = Cm Int | Inch Int
    deriving Show

parseHeight :: Parser Height
parseHeight = choice $ map try [parseCentimeters, parseInches]
    where
        parseCentimeters = do
            x <- parseInt
            string "cm"
            return $ Cm x
        parseInches = do
            x <- parseInt
            string "in"
            return $ Inch x

parseIntFromDigits :: Int -> Parser Int
parseIntFromDigits n = do
    s <- many1 digit
    if length s /= n
      then fail "Invalid digits"
      else return $ read s

parseInt :: Parser Int    
parseInt = do
    s <- many1 digit
    return $ read s


-- parsePassportsString :: Parser Passport
-- parsePassportsString = do
--     sepBy1 parseField ((string "\n\n") <|> string " " <|> string "\n")

-- parsePassportString :: Parser String
-- parsePassportString = do
--     y <- sepBy1 (many anyChar) emptyLine
--     -- x <- many anyChar
--     -- emptyLine <|> eof
--     return y

-- parseContentsCheating :: String -> IO [Either ParseError Passport]
-- parseContentsCheating contents = do
--     putStrLn $ show $ lines contents
--     printAny $ map length $ lines contents
--     putStrLn $ show passportStrings
--     printAny $ length passportStrings
--     putStrLn $ passportStrings !! 0
--     return $ parseResults
--     where 
--         passportStrings = splitPassportStrings contents
--         parseResults = map (\x -> parse parsePassport "" x) passportStrings

-- splitPassportStrings :: String -> [String]
-- splitPassportStrings contents = map unlines $ splitOn "" $ lines contents

-- splitOn :: Eq a => a -> [a] -> [[a]]
-- splitOn sep l = splitOn_ sep l ([]::[[a]])
--     where 
--         splitOn_ :: Eq a => a -> [a] -> [[a]] -> [[a]]
--         splitOn_ _ [] acc = acc
--         splitOn_ sep (x:xs) acc =
--             case acc of
--                 [] -> 
--                     (if sep == x 
--                      then splitOn_ sep xs []
--                      else splitOn_ sep xs [[x]])
--                 (current:others) ->
--                     if sep == x
--                         then splitOn_ sep xs ([]:(current:others))
--                         else splitOn_ sep xs $ [(x:current)] ++ others
