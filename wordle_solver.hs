import qualified Data.Text as T

type WordleLetter = (Char, Int)

main = do
    let x = generateWords "abcdef"
    print x

(|>) :: a -> (a -> b) -> b
(|>) x f = f x
infixl 1 |>

(||>) x f = x |> uncurry f
infixl 1 ||>

(|->) :: (a -> b) -> (b -> c) -> a -> c
(|->) = flip (.)

(|=>) f1 f2 = f1 |-> uncurry f2
infixl 1 |=>

generateWords :: String -> [String]
generateWords bads = do
    let alphabet = "abcdefghijklmnopqrstuvwxyz"
    let chars = alphabet |> filter (`notElem` bads)
    sequence [chars, chars, chars, chars, chars]

fitsAllGreens :: String -> [WordleLetter] -> Bool
fitsAllGreens = all . uncurry . atIndex

fitsYellow :: String -> [Int] -> WordleLetter -> Bool
fitsYellow word unknowns (char, pos) =
    not $ atIndex word char pos && atAnyIndex word char unknowns

fitsAllYellows :: String -> [Int] -> [WordleLetter] -> Bool
fitsAllYellows = curry $ all . uncurry fitsYellow

atAnyIndex :: String -> Char -> [Int] -> Bool
atAnyIndex = curry $ any . uncurry atIndex

atIndex :: String -> Char -> Int -> Bool
atIndex word c pos = word !! pos == c

lettersInWord ::String -> [Char] -> Bool 
lettersInWord = any . flip elem

checkWord :: [WordleLetter]
          -> [WordleLetter]
          -> [Int]
          -> [Char]
          -> String
          -> Bool
checkWord greens yellows unknown bads word = do
    let check1 = not $ lettersInWord word bads
    let check2 = fitsAllGreens word greens
    let check3 = fitsAllYellows word unknown yellows
    check1 && check2 && check3

filterWords :: [WordleLetter]
            -> [WordleLetter]
            -> [Int]
            -> [Char]
            -> [String]
            -> [String]
filterWords greens yellows unknown bads =
    filter (checkWord greens yellows unknown bads)

splitOnStr :: String -> String -> [String]
splitOnStr delim str = str |> T.pack |> T.splitOn (T.pack delim) |> map T.unpack

pairs :: String -> [(Char, Char)]
pairs = splitOnStr " = " |-> (\[x, y] -> (x,y)) |=> zip