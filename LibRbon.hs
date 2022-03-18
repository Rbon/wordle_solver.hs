module LibRbon where

import qualified Data.Text as T

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)
infixl 1 |>

(||>) :: (a, b) -> (a -> b -> c) -> c
(||>) x f = x |> uncurry f
infixl 1 ||>

(|->) :: (a -> b) -> (b -> c) -> a -> c
(|->) = flip (.)
infixl 1 |->

(|=>) :: (a -> (b, c)) -> (b -> c -> d) -> a -> d
(|=>) f g = f |-> uncurry g
infixl 1 |=>

splitOnString :: String -> String -> [String]
splitOnString delim = T.pack |-> T.splitOn (T.pack delim) |-> map T.unpack