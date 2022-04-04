import LibRbon
import Text.Printf ()
import Data.Function (on)
import Data.Sequence (takeWhileR, index)
import GHC.Exts.Heap (GenClosure(what_next))

data WordleLetter = WordleLetter {
    char     :: Char       ,
    inOneOf  :: Maybe [Int],
    mustBeIn :: Maybe Int  ,
    cantBeIn :: Maybe Int  }
    deriving (Show)

type State = ([WordleLetter], [String])

main :: IO ()
main = interactiveStep []

interactiveStep :: [String] -> IO ()
interactiveStep wordList = do
    command <- prompt "guess: "
    let newWordList = step command wordList
    fancyPrint newWordList
    interactiveStep newWordList

fancyPrint :: [String] -> IO ()
fancyPrint wordList
    | length wordList > 99 = print (length wordList)
    | otherwise            = print wordList

initStep :: String -> [String]
initStep command = step command (generateWords $ commandToLetters command)

step :: String -> [String] -> [String]
step command []       = initStep command
step command wordList = filterWords (commandToLetters command) wordList

initWords :: String -> [String]
initWords = generateWords . commandToLetters

commandToLetters :: String -> [WordleLetter]
commandToLetters command =
    let pairs = uncurry zip . (\[x, y] -> (x,y)) . splitOnString " = "
        indexedPairs = insertIndex $ pairs command
        unknowns = pairsToUnknowns indexedPairs
        yellows = generateYellows indexedPairs unknowns
        greens = generateGreens indexedPairs
        wrongs = generateWrongs indexedPairs
    in  greens ++ yellows ++ wrongs

generateGreens :: [(Char, Char, Int)] -> [WordleLetter]
generateGreens indexedPairs =
    let pred (_, color, _) = color == 'g'
        filteredPairs = filter pred indexedPairs
        f (char, _, index) =
            WordleLetter {
                char = char,
                cantBeIn = Nothing,
                mustBeIn = Just index,
                inOneOf  = Nothing}
    in  map f filteredPairs

generateYellows :: [(Char, Char, Int)] -> [Int] -> [WordleLetter]
generateYellows indexedPairs unknowns =
    let pred (_, color, _) = color == 'y'
        filteredPairs = filter pred indexedPairs
        realUnknowns = unknowns
        f (char, color, index) =
            WordleLetter {
                char = char,
                cantBeIn = Just index,
                inOneOf  = Just (remove unknowns index),
                mustBeIn = Nothing}
    in  map f filteredPairs

generateWrongs :: [(Char, Char, Int)] -> [WordleLetter]
generateWrongs indexedPairs =
    let pred (_, color, _) = color == 'w'
        filteredPairs = filter pred indexedPairs
        f (char, _, index) =
            WordleLetter {
                char     = char,
                cantBeIn = Just index,
                mustBeIn = Nothing,
                inOneOf  = Nothing}
    in  map f filteredPairs

pairsToUnknowns :: [(Char, Char, Int)] -> [Int]
pairsToUnknowns =
    filter (\(_, color, _) -> color /= 'g')
    |-> map (\(_, _, i) -> i)

insertIndex :: [(Char, Char)] -> [(Char, Char, Int)]
insertIndex pairs =
    let f (x, y) n = (x, y, n)
        indexes = map (flip (-) 1) [1 .. length pairs]
    in  zipWith f pairs indexes

isPossibleWord :: [WordleLetter] -> String -> Bool
isPossibleWord ls s =
    let atLeastOneElem c s xs = any ((== c) . (s !!)) xs
        matchOneOf s c Nothing   = True 
        matchOneOf s c (Just xs) = atLeastOneElem c s xs
        matchCant  s c Nothing   = True
        matchCant  s c (Just i)  = s !! i /= c
        matchMust  s c Nothing   = True
        matchMust  s c (Just i)  = s !! i == c
        f s l =
            matchOneOf s (char l) (inOneOf l)
            && matchCant s (char l) (cantBeIn l)
            && matchMust s (char l) (mustBeIn l)
    in  all (f s) ls

filterWords :: [WordleLetter] -> [String] -> [String]
filterWords = filter . isPossibleWord

pairs :: String -> [(Char, Char)]
pairs = uncurry zip . (\[x, y] -> (x,y)) . splitOnString " = "

generateWords :: [WordleLetter] -> [String]
generateWords letters = sequence $ f letters where
    f letters = filterPossibleChars letters initPossibles

filterPossibleChars :: [WordleLetter] -> [String] -> [String]
filterPossibleChars letters = filterCants cants . filterMusts musts where
    musts = collectMusts letters
    cants = collectCants letters

filterMusts:: [[Char]] -> [String] -> [String]
filterMusts = zipWith f where
    f "" str = str
    f xs str = xs

filterCants :: [[Char]] -> [String] -> [String]
filterCants = zipWith f where
    f "" str = str
    f xs str = foldl remove str xs

collectMusts :: [WordleLetter] -> [[Char]]
collectMusts = foldl1 (zipWith (++)) . map h where
    initList = [[], [], [], [], []]
    f xs c i = replace xs (i, [c])
    g xs c Nothing  = xs
    g xs c (Just i) = f xs c i
    h letter = g initList (char letter) (mustBeIn letter)

collectCants :: [WordleLetter] -> [[Char]]
collectCants = foldl1 (zipWith (++)) . map h where
    h letter = g initList (char letter) (cantBeIn letter)
    g xs c Nothing  = xs
    g xs c (Just i) = f xs c i
    f xs c i = replace xs (i, [c])
    initList = [[], [], [], [], []]

-- stolen from stackoverflow
replace :: (Num a, Ord a) => [b] -> (a, b) -> [b]
replace [] _ = []
replace (_:xs) (0,a) = a:xs
replace (x:xs) (n,a) =
  if n < 0
    then x:xs
    else x: replace xs (n-1,a)

initPossibles :: [String]
initPossibles = [
    "abcdefghijklmnopqrstuvwxyz" ,
    "abcdefghijklmnopqrstuvwxyz" ,
    "abcdefghijklmnopqrstuvwxyz" ,
    "abcdefghijklmnopqrstuvwxyz" ,
    "abcdefghijklmnopqrstuvwxyz" ]