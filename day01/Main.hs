module Main where
import System.Environment (getArgs)
import Data.List (find, isPrefixOf)
import Data.Char (isDigit, digitToInt)
import Data.Maybe (fromMaybe)

getCalVal :: String -> Int
getCalVal str = findFirstDigit str * 10 + findLastDigit str
  where
    findFirstDigit str = digitToInt $ head $ findAllDigits str
    findLastDigit str = digitToInt $ last $ findAllDigits str
    findAllDigits str = filter isDigit $ wordsToDigits str

wordsToDigits :: String -> String
wordsToDigits [] = []
wordsToDigits str =
  case maybePrefixNum str of
    Nothing -> head str : wordsToDigits (tail str)
    Just (numChar, str') -> numChar : wordsToDigits str'

maybePrefixNum :: String -> Maybe (Char, String)
maybePrefixNum str =
  let nums = [(snum, num) | (snum, num) <- wordToNum, snum `isPrefixOf` str]
  in prefixNums nums
  where
    prefixNums [] = Nothing
    prefixNums ((snum, num):nums) = Just (num, tail str)

wordToNum :: [(String, Char)]
wordToNum = [
  ("zero", '0'),
  ("one", '1'),
  ("two", '2'),
  ("three", '3'),
  ("four", '4'),
  ("five", '5'),
  ("six", '6'),
  ("seven", '7'),
  ("eight", '8'),
  ("nine", '9')]

getLinesFromFile :: FilePath -> IO [String]
getLinesFromFile filepath = do
  content <- readFile filepath
  return $ lines content

main :: IO ()
main = do
  inputs <- getArgs >>= \args -> getLinesFromFile $ head args

  print $ sum $ map getCalVal inputs
