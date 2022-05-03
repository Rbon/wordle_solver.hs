-- TODO:
--     nicer output
--     usage text

-- import Text.Printf ()
import LibRbon (prompt)
import qualified Backend
import qualified WordList as W
import Data.List (sort)

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
                let newWords = Backend.step words userInput
                print newWords
                f newWords

