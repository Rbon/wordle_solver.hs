{-# LANGUAGE TupleSections #-}
module Backend where
import System.IO ( stdout, hFlush )
import qualified Data.Text as T

import Data.Function ( on )

type Pair = (Char, Char)
type Info = (Matcher, [Char])
type Matcher = String -> [Char] -> Bool

instance Show (a -> b) where
         show a = "function"

-- | Compose two functions.
-- except that @g@ will be fed /two/ arguments instead of one
-- before handing its result to @f@.
(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f ... g) x y = f (g x y)
infixr 8 ...

remove :: Eq a => a -> [a] -> [a]
remove = filter . (/=)

prompt :: String -> IO String
prompt text = putStr text >> hFlush stdout >> getLine

splitOnString :: String -> String -> [String]
splitOnString delim = map T.unpack . T.splitOn (T.pack delim) . T.pack

step :: [String] -> String -> [String]
step = flip $ filterWords . generateInfo . generateGuess

generatePairs :: String -> [Pair]
generatePairs = uncurry zip . (\[x, y] -> (x,y)) . splitOnString " = "

generateGuess :: String -> (String, String)
generateGuess = (\[x, y] -> (x,y)) . splitOnString " = "

generateGreens :: (String, String) -> [Info]
generateGreens (word, result) = [(matchAll, zipWith f word result)] where
    f char1 char2 = if char2 == 'g' then char1 else '-'

generateWrongs :: (String, String) -> [Info]
generateWrongs (word, result) = map (matchNone,) properWrongs where
    wrongs' = filter (/= '-') $ generateWrongs' (word, result)
    yellows = filter (/= '-') $ generateYellows' (word, result)
    wrongs = accountForYellows wrongs' yellows
    unknowns = generateUnknowns (word, result)
    properWrongs = map (propagateLetter unknowns) wrongs

accountForYellows :: String -> String -> String
accountForYellows wrongs yellows = filter (f yellows) wrongs where
    f yellows wrong = wrong `notElem` yellows

generateWrongs' :: (String, String) -> String
generateWrongs' (word, result) = zipWith f word result where
    f char1 char2 = if char2 == 'w' then char1 else '-'

propagateLetter :: String -> Char -> String
propagateLetter str char = map (f char) str where
    f char1 char2 = if char2 /= '-' then char1 else '-'

generateUnknowns :: (String, String) -> String
generateUnknowns (word, result) = zipWith f word result where
    f char1 char2 = if char2 /= 'g' then char1 else '-'

generateYellows :: (String, String) -> [Info]
generateYellows (word, result) = output where
    wrongs = generateYellows' (word, result)
    yellows = filter (/= '-') $ generateYellows' (word, result)
    unknowns = generateUnknowns (word, result)
    properYellows = map (propagateLetter unknowns) yellows
    properYellows' = map (matchAny,) $ excludeWrong wrongs properYellows
    output = (matchNone, wrongs) : properYellows'

generateYellows' :: (String, String) -> String
generateYellows' (word, result) = zipWith f word result where
    f char1 char2 = if char2 == 'y' then char1 else '-'

excludeWrong :: String -> [String] -> [String]
excludeWrong = map . zipWith f where
    f '-' chr2 = chr2
    f chr1 chr2 = if chr1 /= chr2 then chr2 else '-'

generateInfo :: (String, String) -> [Info]
generateInfo guess =
    generateGreens guess
    ++ generateWrongs guess
    ++ generateYellows guess

needsToBeMatched :: String -> [Char] -> String
needsToBeMatched = zipWith f where
    f _    '-' = '-'
    f char  _  = char

matchAny :: String -> [Char] -> Bool
matchAny = or ... zipWith (==) `on` filter (/= '-')

matchNone :: String -> [Char] -> Bool
matchNone = not ... matchAny

matchAll :: String -> [Char] -> Bool
matchAll = and ... zipWith (==) `on` filter (/= '-')

checkAgainst :: String -> (Matcher, [Char]) -> Bool
checkAgainst str (matcher, chrs) = matcher (needsToBeMatched str chrs) chrs

checkAgainstAll :: String -> [(Matcher, [Char])] -> Bool
checkAgainstAll = all . checkAgainst

filterWords ::[Info] -> [String] -> [String]
filterWords = filter . flip checkAgainstAll

--------------------------------------------------------------------------------
-- beyond here is deprecated stuff,
-- using generated words instead of a master list

initPossibles :: [String]
initPossibles = [
    "abcdefghijklmnopqrstuvwxyz" ,
    "abcdefghijklmnopqrstuvwxyz" ,
    "abcdefghijklmnopqrstuvwxyz" ,
    "abcdefghijklmnopqrstuvwxyz" ,
    "abcdefghijklmnopqrstuvwxyz" ]

-- initStep :: String -> [String]
-- initStep command = newWords where
--     p = generatePairs command
--     info = generateInfo command
--     newPossibles = filterPossibleChars p initPossibles
--     newWords = generateWords info newPossibles

filterPossibleChars :: [Pair] -> [String] -> [String]
filterPossibleChars = zipWith filter1 where
    filter1 (char, 'g') _   = [char]
    filter1 (char, _  ) str = remove char str

-- generateWords :: Info -> [String] -> [String]
-- generateWords pairs = filterWords pairs . sequence