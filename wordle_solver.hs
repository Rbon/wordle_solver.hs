-- TODO:
--      nicer output
--      refactors
--      usage text

-- import Text.Printf ()

import Backend
import qualified WordList as W
import Data.List (sort)
import System.IO ( stdout, hFlush )
import LibRbon (prompt)

main :: IO ()
main = f [] where
    f words = do
        if null words then
            f $ sort W.allWords -- this sort is here to minimize cheatiness
        else do
            userInput <- prompt "guess: "
            if userInput == "quit" then
                putStrLn "quitting"
            else do
                let newWords = step words userInput
                print newWords
                f newWords

