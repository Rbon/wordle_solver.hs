module LibRbon where

import qualified Data.Text as T

import System.IO ( stdout, hFlush )

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

notEquals :: Eq a => a -> a -> Bool
notEquals = (/=)

remove :: Eq a => [a] -> a -> [a]
remove = flip $ filter . (/=)

(!=) :: Eq a => a -> a -> Bool
(!=) = (/=)
infixl 1 !=

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)
infixl 1 |>

(||>) :: (a, b) -> (a -> b -> c) -> c
(||>) x f = x |> uncurry f
infixl 1 ||>

(|->) :: (a -> b) -> (b -> c) -> a -> c
(|->) = flip (.)
infixl 1 |->

(<-|) :: (b -> c) -> (a -> b) -> a -> c
(<-|) = (.)

(|=>) :: (a -> b -> c) -> (c -> d) -> a -> b -> d
(|=>) = flip (...)
infixl 1 |=>


splitOnString :: String -> String -> [String]
splitOnString delim = T.pack |-> T.splitOn (T.pack delim) |-> map T.unpack

-- | Compose two functions.
-- except that @g@ will be fed /two/ arguments instead of one
-- before handing its result to @f@.
(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f ... g) x y = f (g x y)
infixr 8 ...


-- think of this as a pipe that feeds the right function two arguments
-- (<=|) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
-- (<=|) = (.) . (.)
-- infixl 1 <=|