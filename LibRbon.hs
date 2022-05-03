module LibRbon where

import qualified Data.Text as T
import System.IO ( stdout, hFlush )
import Data.Function ( on )

prompt :: String -> IO String
prompt text = putStr text >> hFlush stdout >> getLine

remove :: Eq a => a -> [a] -> [a]
remove = filter . (/=)

-- | This function exists because of JetBrains Mono ligatures
(!=) :: Eq a => a -> a -> Bool
(!=) = (/=)
infixl 1 !=

-- | Wrapper for 'splitOn' from Data.Text, to use 'Strings' instead of 'Texts'.
-- Break a 'String' into pieces separated by the first 'String'
-- argument (which cannot be empty), consuming the delimiter. An empty
-- delimiter is invalid, and will cause an error to be raised.
--
-- Examples:
--
-- >>> splitOn "\r\n" "a\r\nb\r\nd\r\ne"
-- ["a","b","d","e"]
--
-- >>> splitOn "aaa"  "aaaXaaaXaaaXaaa"
-- ["","X","X","X",""]
--
-- >>> splitOn "x"    "x"
-- ["",""]
splitOn :: String -> String -> [String]
splitOn = map T.unpack ... T.splitOn `on` T.pack

-- | Shamelessly stolen from 'Data.Composition', and renamed to '...'.
-- Compose two functions. @f ... g@ is similar to @f . g@
-- except that @g@ will be fed /two/ arguments instead of one
-- before handing its result to @f@.
--
-- Example usage:
--
-- > concatMap :: (a -> [b]) -> [a] -> [b]
-- > concatMap = concat ... map
--
-- Notice how /two/ arguments
-- (the function /and/ the list)
-- will be given to @map@ before the result
-- is passed to @concat@. This is equivalent to:
--
-- > concatMap f xs = concat (map f xs)
(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) . (.)
infixr 8 ...

-- think of this as a pipe that feeds the right function two arguments
-- (<=|) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
-- (<=|) = (.) . (.)
-- infixl 1 <=|