module Main where
import Text.Read (readMaybe)
import Data.Maybe (fromJust, fromMaybe)

data Pull = RGB Int Int Int deriving Show
data Game = Game Int [Pull] deriving Show

isPullPossible :: Pull -> Bool
isPullPossible (RGB r g b) = r < 13 && g < 14 && b  < 15

isGamePossible :: Game -> Bool
isGamePossible (Game _ pulls) = all isPullPossible pulls

parseColor :: [String] -> (String, Int, Bool, [String])
parseColor (nStr:colStr:toks) =
  let
    Just n = readMaybe nStr :: Maybe Int
    col = init colStr
    sep = last colStr
  in
    if sep == ',' || sep == ';' then
      (col, n, sep==',', toks)
    else
      (colStr, n, False, toks)

parseColors :: [String] -> ([(String, Int)], [String])
parseColors toks =
  let (col, n, more, toks') = parseColor toks
  in
    if more then
      let (cols, toks'') = parseColors toks'
      in ((col, n):cols, toks'')
    else
      ([(col, n)], toks')

parsePull :: [String] -> (Pull, [String])
parsePull toks =
  let (cols, toks') = parseColors toks
  in (RGB (getCol "red" cols) (getCol "green" cols) (getCol "blue" cols), toks')
  where
    getCol name cols = fromMaybe 0 $ lookup name cols

parsePulls :: [String] -> [Pull]
parsePulls [] = []
parsePulls toks =
  let (pull, toks') = parsePull toks
  in pull : parsePulls toks'

parseGame :: [String] -> Game
parseGame (gameStr:idStr:pullToks)
  | gameStr == "Game" =
    let
      Just id = readMaybe (init idStr) :: Maybe Int
      pulls = parsePulls pullToks
    in
      Game id pulls

maxPull :: [Pull] -> Pull
maxPull [] = RGB 0 0 0
maxPull ((RGB r1 g1 b1):pulls) =
  let (RGB r2 g2 b2) = maxPull pulls
  in RGB (max r1 r2) (max g1 g2) (max b1 b2)

gamePower :: Game -> Int
gamePower (Game _ pulls) =
  let (RGB r g b) = maxPull pulls
  in r*g*b

main :: IO ()
main = do
  content <- readFile "input.txt"
  let inputs = lines content
  let games = map (parseGame . words) inputs
  --let possibleGameIds = [(\(Game id _) -> id) game | game <- games, isGamePossible game]
  --print $ sum possibleGameIds
  print $ sum $ map gamePower games
