--- module (NICHT AENDERN!)
module CannonBot where
--- imports (NICHT AENDERN!)
import           Data.Char
import           Util

--- external signatures (NICHT AENDERN!)
getMove :: String -> String
listMoves :: String -> String

--- YOUR IMPLEMENTATION STARTS HERE ---
getMove a = a
listMoves b = b

split :: String -> [String]
split [] = [""]
split (c:cs)
   | c == '/'  = "" : rest
   | c == ' ' = "" : rest
   | otherwise = (c : head rest) : tail rest
 where
   rest = split cs

splitted = split "4W5/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w///b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/7B2 w"

selectPlayer :: [String] -> Player
selectPlayer xs = if (xs !! 10 == "b") then Black else White

data Player = Black | White
instance Show Player where
 show Black =  "Black"
 show White =  "White"

trimLoop :: Int -> [Int] -> String
trimLoop 0 l = intListToString l
trimLoop n l = trimLoop (n-1) ([1] ++ l)

intListToString :: [Int] -> String
intListToString = map (\i -> chr (i + ord '0'))

stringSpace x = trimLoop x []

charToString :: Char -> String
charToString = (:[])

trimMapper :: String -> String
trimMapper c = if c=="-" then stringSpace 10
  else if (c /= "w" && c /= "W" && c /= "b" && c /= "B") then stringSpace (digitToInt (head c))
    else c

stringReplacer :: String -> String
stringReplacer [] = []
stringReplacer x = trimMapper (charToString (head x)) ++ (stringReplacer (tail x))

fillEmpty :: String -> String
fillEmpty [] = "-"
fillEmpty s  = s

doFillUp = map fillEmpty splitted
doSomething = map stringReplacer doFillUp

dropPlayer :: [String] -> Int -> [String]
dropPlayer xs 0 = []
dropPlayer xs n = [head xs] ++ dropPlayer (tail xs) (n-1)

--- GHC Commands---
getPlayer = selectPlayer splitted
finalList = dropPlayer doSomething 10
