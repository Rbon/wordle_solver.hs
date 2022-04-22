-- TODO: clean up generateYellows
--       use a plain text file instead of WordList

-- import Text.Printf ()
import System.IO ( stdout, hFlush )
import qualified Data.Text as T
import qualified WordList as W

type Pair = (Char, Char)
type Green = Char
type Wrong = Char
type Yellow = (Char, [Int])
type Info = ([Green], [Yellow], [Wrong])

-- | Compose two functions.
-- except that @g@ will be fed /two/ arguments instead of one
-- before handing its result to @f@.
(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f ... g) x y = f (g x y)
infixr 8 ...

remove :: Eq a => a -> [a] -> [a]
remove = filter . (/=)

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

splitOnString :: String -> String -> [String]
splitOnString delim = map T.unpack . T.splitOn (T.pack delim) . T.pack

main :: IO ()
main = interactiveStep "" W.allWords

generatePairs :: String -> [Pair]
generatePairs = uncurry zip . (\[x, y] -> (x,y)) . splitOnString " = "

interactiveStep :: String -> [String] -> IO ()
interactiveStep "" wordList = do
    command <- prompt "guess: "
    interactiveStep command wordList

interactiveStep "quit" wordList = do
    putStr ""

interactiveStep command wordList = do
    let newWordList = step command wordList
    print newWordList
    newCommand <- prompt "guess: "
    interactiveStep newCommand newWordList

generateGreens :: [Pair] -> [Char]
generateGreens = map format where
    format (char, color)
        | color == 'g' = char
        | otherwise    = '-'

matchGreens :: String -> [Char] -> Bool
matchGreens = and ... zipWith matcher where
    matcher char '-' = True
    matcher char green = char == green

generateWrongs :: [Pair] -> [Char]
generateWrongs = map format . filter pred where
    pred (_, color) = color == 'w'
    format (char, _) = char

matchWrongs :: String -> [Char] -> Bool
matchWrongs = all . flip notElem

generateWrongs' :: [Pair] -> [String]
generateWrongs' pairs = map (propagateWrong (greens pairs)) (wrongs pairs) where
    greens pairs = map fst (filter isGreen pairs)
    wrongs pairs = map fst (filter isWrong pairs)
    isGreen (_, color) = color == 'g'
    isWrong (_, color) = color == 'w'
    propagateWrong greens wrong = zipWith f (allWrong greens wrong) greens
    f wrong '-' = wrong
    f _      _  = '-'
    allWrong greens wrong = replicate (length greens) wrong



-- "idiot = wygyw"
-- [('i','w'),('d','y'),('i','g'),('o','y'),('t','w')]
-- generateWrongs = [
--     "ii-ii"
--     "tt-tt"
-- ]
-- populateWrongs = "ii-ii"

generateYellows :: [Pair] -> [(Char, [Int])]
generateYellows pairs = map properYellow allYellows where
    indexedPairs = insertIndex pairs
    combinePairList = zipWith (++)
    -- known = combinePairList (map f pairs) greens
    isYellow  (_, color, _) = color == 'y'
    isUnknown (_, color, _) = color /= 'g'
    onlyIndex (_, _    , n) = n

    allYellows = filter isYellow indexedPairs
    allUnknowns = map onlyIndex $ filter isUnknown indexedPairs
    properYellow (char, color, n) = (char, remove n allUnknowns )

    insertIndex pairs = zipWith f pairs indexes
    f (x, y) n = (x, y, n)
    indexes = [0 .. (length pairs - 1)]

matchYellows :: String -> [(Char, [Int])] -> Bool
matchYellows = all . matchOneYellow where
    matchOneYellow str (char, ns) = any (g str char) ns
    g str char n = str !! n == char

generateInfo :: String -> Info
generateInfo command = (greens, yellows, wrongs) where
    pairs = generatePairs command
    greens = generateGreens pairs
    yellows = generateYellows pairs
    wrongs = generateWrongs pairs

matchInfo :: Info -> String -> Bool
matchInfo(greens, yellows, wrongs) str  =
    matchesGreen && matchesYellow && matchesWrong where
    matchesGreen = matchGreens str greens
    matchesYellow = matchYellows str yellows
    matchesWrong = matchWrongs str wrongs

step :: String -> [String] -> [String]
step command wordList = newWords where
    info = generateInfo command
    newWords = filterWords info wordList

filterWords :: Info ->  [String] -> [String]
filterWords = filter . matchInfo

-- beyond here is deprecated stuff,
-- using generated words instead of a master list

initPossibles :: [String]
initPossibles = [
    "abcdefghijklmnopqrstuvwxyz" ,
    "abcdefghijklmnopqrstuvwxyz" ,
    "abcdefghijklmnopqrstuvwxyz" ,
    "abcdefghijklmnopqrstuvwxyz" ,
    "abcdefghijklmnopqrstuvwxyz" ]

initStep :: String -> [String]
initStep command = newWords where
    p = generatePairs command
    info = generateInfo command
    newPossibles = filterPossibleChars p initPossibles
    newWords = generateWords info newPossibles

filterPossibleChars :: [Pair] -> [String] -> [String]
filterPossibleChars = zipWith filter1 where
    filter1 (char, 'g') _   = [char]
    filter1 (char, _  ) str = remove char str

generateWords :: Info -> [String] -> [String]
generateWords pairs = filterWords pairs . sequence