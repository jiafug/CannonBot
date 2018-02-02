--- module (NICHT AENDERN!)
module CannonBot where
--- imports (NICHT AENDERN!)
import Data.Char
import Util

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

--- GHC Commands---
splitted = split "4W5/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w///b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/7B2 w"

selectPlayer :: [String] -> Player
selectPlayer xs = if (xs !! 10 == "b") then Black else White

data Player = Black | White
instance Show Player where
 show Black =  "Black"
 show White =  "White"
 
--- GHC Commands---
getPlayer = selectPlayer splitted

trimLoop :: Integer -> [Int] -> String
trimLoop 0 l = intListToString l
trimLoop n l = trimLoop (n-1) ([1] ++ l) 

intListToString :: [Int] -> String
intListToString = map (\i -> chr (i + ord '0'))

--- GHC Commands---
stringSpace x = trimLoop x []

--- GHC Commands---
doSomething = map trimString splitted

trimMapper :: Char -> String
trimMapper c = if (c /= 'w' && c /= 'W' && c /= 'b' && c /= 'B') then stringSpace (toInteger (digitToInt c)) else return c

---Hier weitermachen---
trimString :: String -> String
trimString s =  map (\x -> trimMapper x)

