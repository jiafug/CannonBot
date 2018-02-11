--- module (NICHT AENDERN!)
module CannonBot where

--- imports (NICHT AENDERN!)
import           Data.Char
import           Util

-----------------------------------------------------------
---------------Bei der Abgabe zu entfernen!!!!-------------
import           Data.List
--------------Bei der Abgabe zu entfernen!!!!--------------
-----------------------------------------------------------

--- external signatures (NICHT AENDERN!)
getMove :: String -> String
listMoves :: String -> String

--- YOUR IMPLEMENTATION STARTS HERE ---

-----------------------------------------------------------
---------------Bei der Abgabe zu entfernen!!!!-------------
splitted = split "4W5/1w1w3w1w/1w1w1w1w1w/1w2w2w1w/3wb5/2b1w5/b5b1b1/b1bbbbb1b1/b3b3b1/4B5 b"
doFillUp = map fillEmpty splitted
doSomething = map stringReplacer doFillUp
getPlayer = selectPlayer splitted
finalList = dropPlayer doSomething 10
testCombi = listCombi listItar
test = map toMoveString (filter (\x -> toCheck finalList x getPlayer) testCombi)
--------------Bei der Abgabe zu entfernen!!!!--------------
-----------------------------------------------------------

getMove a =
  let
    list = filter (not . null) (split (listMoves a))
  in
    list !! 0

listMoves b =
  let
    splitted = split b
    doFillUp = map fillEmpty splitted
    doSomething = map stringReplacer doFillUp
    getPlayer = selectPlayer splitted
    finalList = dropPlayer doSomething 10
    testCombi = listCombi listItar
    test = map toMoveString (filter (\x -> toCheck finalList x getPlayer) testCombi)
  in
    if (searchForTown b getPlayer)
      then "[" ++ join "," test ++ "]"
      else "[" ++ join "," (validTown getPlayer) ++ "]"

-- Funktion, die alle erlaubten Stadtposition zurückgibt
validTown :: Player -> [String]
validTown White {} = ["b9-b9", "c9-c9", "d9-d9", "e9-e9", "f9-f9", "g9-g9", "h9-h9", "i9-i9"]
validTown Black {} = ["b0-b0", "c0-c0", "d0-d0", "e0-e0", "f0-f0", "g0-g0", "h0-h0", "i0-i0"]

-- Funktion, die untersucht, ob die Stadt schon gesetzt wurde
searchForTown :: String -> Player -> Bool
searchForTown s p =
  if ((length (filter (\x -> x == town p) s)) > 0)
    then True
    else False

-- Funktion, die einen gegbenen FEN-String bei '/' oder ' ' teilt und als eine Liste von Strings speichert
split :: String -> [String]
split [] = [""]
split (c:cs)
   | c == '/'  = "" : rest
   | c == ' ' = "" : rest
   | c == ',' = "" : rest
   | c == '[' = "" : rest
   | c == ']' = "" : rest
   | otherwise = (c : head rest) : tail rest
 where
   rest = split cs

-- Funktion, die anhand einer FEN-String-Liste (split) den aktuellen Spieler ermittelt
selectPlayer :: [String] -> Player
selectPlayer xs =
  if (xs !! 10 == "b")
    then Black 'b' 'B' 'w' 'W'
    else White 'w' 'W' 'b' 'B'

-- Datatype Spieler
data Player = Black {soldier :: Char, town :: Char, eSoldier :: Char, eTown :: Char}
  | White {soldier :: Char, town :: Char, eSoldier :: Char, eTown :: Char}

-- Funktion, die einen gegbenen Int-Wert zu einem String lauter Einsen macht
trimLoop :: Int -> [Int] -> String
trimLoop 0 l = intListToString l
trimLoop n l = trimLoop (n-1) ([1] ++ l)

-- Funktion, die eine Liste von Ints zu eine rkonkatenierten String zusammenfügt
intListToString :: [Int] -> String
intListToString = map (\i -> chr (i + ord '0'))

-- Standardaufruf der Funktion trimLoop
stringSpace x = trimLoop x []

-- Funktion, die einen gegbenen Char in einen String verwandelt
charToString :: Char -> String
charToString = (:[])

-- Funktion, die ein String ersetzt mit Ergebnissen aus trimLoop
trimMapper :: String -> String
trimMapper c = if c=="-"
  then stringSpace 10
  else if (c /= "w" && c /= "W" && c /= "b" && c /= "B")
    then stringSpace (digitToInt (head c))
    else c

-- Funktion, die rekusiv trimMapper mit einzelnen Strings aufruft
stringReplacer :: String -> String
stringReplacer [] = []
stringReplacer x = trimMapper (charToString (head x)) ++ (stringReplacer (tail x))

-- Funktion, die die Reihen ohne Figuren mit einem "-" ausfüllt, als Platzhalter
fillEmpty :: String -> String
fillEmpty [] = "-"
fillEmpty s  = s

-- Funktion, die den aktullen Spieler aus der stringReplacer und fillEmpty Liste entfernt
dropPlayer :: [String] -> Int -> [String]
dropPlayer xs 0 = []
dropPlayer xs n = [head xs] ++ dropPlayer (tail xs) (n-1)

-- Funktion, die die Werte von "00" bis "99" als String-Liste erzeugt
listItar :: [String]
listItar = mapM (const "0123456789") [1..2]

-- Funktion, die alle möglichen Kombinationen einer Liste von Strings erzeugt
listCombi :: [String] -> [[String]]
listCombi s = mapM (const s) [1..2]

-- Funktion, die als filter Signatur dient
toCheck :: [String] -> [String] -> Player -> Bool
toCheck l g p =
  if testFunc g l p
    then True
    else False

-- Funktion, die den Wert des FEN-Strings zurückgibt an der Stelle x, y
getListElement :: [String] -> Int -> Int -> Char
getListElement l x y =
  if (x < 0 || x > 9 || y < 0 || y > 9)
    then 'f'
    else l !! y !! x

-- Funktion, die zwei x und y Koordinaten zu eimen Move-String umwandelt (auch vewendet als map Signatur)
toMoveString :: [String] -> String
toMoveString l = let
  startX = digitToInt (l !! 0 !! 1)
  startY = 9 - (digitToInt (l !! 0 !! 0))
  destX = digitToInt (l !! 1 !! 1)
  destY = 9 - (digitToInt (l !! 1 !! 0))
 in
  charToString (chr (ord 'a' + startX))
  ++ charToString (chr (startY + ord '0'))
  ++ "-"
  ++ charToString (chr (ord 'a' + destX))
  ++ charToString (chr (destY + ord '0'))

-- Funktion, die eine Liste mit den Start- und Zielkoordinaten erhält
testFunc :: [String] -> [String] -> Player -> Bool
testFunc l g p = let
  startX = digitToInt (l !! 0 !! 1)
  startY = digitToInt (l !! 0 !! 0)
  destX = digitToInt (l !! 1 !! 1)
  destY = digitToInt (l !! 1 !! 0)
 in
  if (getListElement g startX startY == soldier p
    && (simpleMove g startX startY destX destY p
      || attackMove g startX startY destX destY p
      || retreatMove g startX startY destX destY p
      || cannonMove g startX startY destX destY p
      || cannonAttack g startX startY destX destY p))
    then True
    else False

-- Funktion, die True zurückgibt, wenn ein simpleMove möglichen ist
simpleMove :: [String] -> Int -> Int -> Int -> Int -> Player -> Bool
simpleMove l sx sy dx dy p =
  if ((getListElement l dx dy == '1' || (getListElement l dx dy == eSoldier p || getListElement l dx dy == eTown p))
      && (dx == sx + 1 || dx == sx - 1 || dx == sx) && ((soldier p == 'b' && dy == sy - 1) || (soldier p == 'w' && dy == sy + 1) ))
    then True
    else False

-- Funktion, die True zurückgibt, wenn ein attackMove möglichen ist
attackMove :: [String] -> Int -> Int -> Int -> Int -> Player -> Bool
attackMove l sx sy dx dy p =
  if ((dx == sx - 1 || dx == sx + 1) && (dy == sy) && (getListElement l dx dy == eSoldier p
      || getListElement l dx dy == eTown p))
    then True
    else False

-- Funktion, die True zurückgibt, wenn ein retreatMove möglichen ist
retreatMove :: [String] -> Int -> Int -> Int -> Int -> Player -> Bool
retreatMove l sx sy dx dy p =
  if (getListElement l dx dy == '1'
    && (((soldier p == 'b' && dy == sy + 2) || (soldier p == 'w' && dy == sy - 2))
      && (getListElement l sx (sy + 1) == eSoldier p || getListElement l (sx - 1) sy == eSoldier p
        || getListElement l (sx - 1) (sy + 1) == eSoldier p || getListElement l (sx + 1) sy == eSoldier p
        || getListElement l (sx + 1) (sy + 1) == eSoldier p || getListElement l sx (sy - 1) == eSoldier p
        || getListElement l (sx - 1) (sy - 1) == eSoldier p || getListElement l (sx + 1) (sy - 1) == eSoldier p))
      && (dx == sx - 2 || dx == sx + 2 || dx == sx)
      && (getListElement l ((dx + sx) `div` 2) ((dy + sy) `div` 2)) == '1')
    then True
    else False

-- Funktion, die True zurückgibt, wenn ein cannonMove möglichen ist
cannonMove :: [String] -> Int -> Int -> Int -> Int -> Player -> Bool
cannonMove l sx sy dx dy p =
  if (getListElement l dx dy == '1' && (((dy - sy == 3) && (dx == sx)
    && getListElement l sx (sy + 1) == soldier p && getListElement l sx (sy + 2) == soldier p
    || (dy - sy == 3) && (dx - sx == 3) && getListElement l (sx + 1) (sy + 1) == soldier p
      && getListElement l (sx + 2) (sy + 2) == soldier p
    || (dy == sy) && (dx - sx == 3) && getListElement l (sx + 1) sy == soldier p
      && getListElement l (sx + 2) sy == soldier p
    || (dy - sy == -3) && (dx - sx == 3) && getListElement l (sx + 1) (sy - 1) == soldier p
      && getListElement l (sx + 2) (sy - 2) == soldier p
    || (dy - sy == -3) && (dx == sx) && getListElement l sx (sy - 1) == soldier p
      && getListElement l sx (sy - 2) == soldier p
    || (dy - sy == -3) && (dx - sx == -3) && getListElement l (sx - 1) (sy - 1) == soldier p
      && getListElement l (sx - 2) (sy - 2) == soldier p
    || (dy == sy) && (dx - sx == -3) && getListElement l (sx - 1) sy == soldier p
      && getListElement l (sx - 2) sy == soldier p
    || (dy - sy == 3) && (dx - sx == -3) && getListElement l (sx - 1) (sy + 1) == soldier p
        && getListElement l (sx - 2) (sy + 2) == soldier p)))
    then True
    else False

-- Funktion, die True zurückgibt, wenn ein cannonMove möglichen ist
cannonAttack :: [String] -> Int -> Int -> Int -> Int -> Player -> Bool
cannonAttack l sx sy dx dy p =
  if (((getListElement l dx dy == eSoldier p || getListElement l dx dy == eTown p)
    && (((dy == sy - 4 || dy == sy - 5) && dx == sx
      && getListElement l sx (sy - 1) == soldier p && getListElement l sx (sy - 2) == soldier p
      && getListElement l sx (sy - 3) == soldier p)
      || (((dy == sy - 4 && dx == sx + 4)
        || (dy == sy - 5 && dx == sx + 5))
        && getListElement l (sx + 1) (sy - 1) == soldier p
        && getListElement l (sx + 2) (sy - 2) == soldier p && getListElement l (sx + 3) (sy - 3) == '1')
      || (dy == sy
        && (dx == sx + 4 || dx == sx + 5) && getListElement l (sx + 1) sy == soldier p
        && getListElement l (sx + 2) sy == soldier p && getListElement l (sx + 3) sy == '1')
      || (((dy == sy + 4 && dx == sx + 4)
        || (dy == sy + 5 && dx == sx + 5))
        && getListElement l (sx + 1) (sy + 1) == soldier p
        && getListElement l (sx + 2) (sy + 2) == soldier p && getListElement l (sx + 3) (sy + 3) == '1')
      || ((dy == sy + 4 || dy == sy + 5)
        && dx == sx && getListElement l sx (sy + 1) == soldier p
        && getListElement l sx (sy + 2) == soldier p && getListElement l sx (sy + 3) == '1')
      || (((dy == sy + 4 && dx == sx - 4)
        || (dy == sy + 5 && dx == sx - 5))
        && getListElement l (sx - 1) (sy + 1) == soldier p
        && getListElement l (sx - 2) (sy + 2) == soldier p && getListElement l (sx - 3) (sy + 3) == '1')
      || (dy == sy && (dx == sx - 4 || dx == sx - 5)
        && getListElement l (sx - 1) sy == soldier p && getListElement l (sx - 2) sy == soldier p
        && getListElement l (sx - 3) sy == soldier p)
      || (((dy == sy - 4 && dx == sx - 4)
        || (dy == sy - 5 && dx == sx - 5))
        && getListElement l (sx - 1) (sy - 1) == soldier p
        && getListElement l (sx - 2) (sy - 2) == soldier p && getListElement l (sx - 3) (sy - 3) == '1'))))
    then True
    else False

-- Funktion, die aus einer Liste einen gemeinsamen String erstellt
join :: String -> [String] -> String
join s l = foldr (\a b-> a ++ if b=="" then b else s ++ b) "" l
