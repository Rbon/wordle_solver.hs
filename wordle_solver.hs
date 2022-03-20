import LibRbon
-- import Words as W
import Text.Printf

-- what is this I never typed this
-- import Distribution.PackageDescription (defaultIncludeRenaming)

type GuessLetter = (Char, Char, Int)

type Guess = [GuessLetter]

type Board = [Guess] 

type PossibleWord = String

type Bad = Char

type Unknown = Int

type Green = WordleLetter

type Yellow = WordleLetter

data State = State {
    wordList    :: [PossibleWord],
    greens   :: [Green],
    yellows  :: [Yellow],
    bads     :: [Bad],
    unknowns :: [Unknown],
    board    :: Board
    } deriving (Show)

type WordleLetter = (Char, Char, Int)

-- main = do
--     let x = generateWords "abcdef"
--     print x

generateWords :: State -> State
generateWords state = do
    let alphabet = "abcdefghijklmnopqrstuvwxyz"
    let chars = alphabet |> filter (`notElem` bads state)
    let words = sequence [chars, chars, chars, chars, chars]
    State words (greens state) (yellows state) (bads state) (unknowns state) (board state)

fitsAllGreens :: String -> [Green] -> Bool
fitsAllGreens = do
    let fitsGreen word (c, _, i) = atIndex word c i
    fitsGreen |-> all

fitsYellow :: String -> [Int] -> WordleLetter -> Bool
fitsYellow word unknowns (char, _, pos) =
    not $ atIndex word char pos && atAnyIndex word char unknowns

fitsAllYellows :: String -> [Int] -> [WordleLetter] -> Bool
fitsAllYellows = curry $ all . uncurry fitsYellow

atAnyIndex :: String -> Char -> [Int] -> Bool
atAnyIndex = curry $ any . uncurry atIndex

atIndex :: String -> Char -> Int -> Bool
atIndex word c pos = word !! pos == c

lettersInWord ::String -> [Char] -> Bool 
lettersInWord = flip elem |-> any

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

filterWords :: State -> State
filterWords (State words greens yellows unknown bads board) = do
    let new_words = filter (checkWord greens yellows bads unknown) words
    State new_words greens yellows unknown bads board

guess :: String -> Guess
guess = do
    let addNumbers n (char, color) = (char, color, n)
    splitOnString " = "
        |-> (\[x, y] -> (x,y))
        |=> zip
        |-> zipWith addNumbers [0..4]

parse :: String -> State
parse command = do
    let letters = command |> guess
    let greens = letters |> filter (\(_, c, _) -> c == 'g')
    let yellows = letters |> filter (\(_, c, _) -> c == 'y')
    let bads = letters
               |> filter (\(_, c, _) -> c == 'b')
               |> map (\(c, _, _) -> c)
    State [] greens yellows bads [] [letters]

updateState :: State -> State -> State
updateState (State w1 g1 y1 ba1 u1 bo1) (State w2 g2 y2 ba2 u2 bo2) = do
    let words = w1 ++ w2
    let greens = g1 ++ g2
    let yellows = y1 ++ y2
    let bads = ba1 ++ ba2
    let unknowns = (g1, g2) ||> determineUnknown
    let board = bo1 ++ bo2
    State words greens yellows bads unknowns board

determineUnknown :: [Green] -> [Green] -> [Unknown]
determineUnknown g1 g2 = do
    let u = [0..4]
    let greenIndexes1 = g1 |> map (\(_, _, x) -> x)
    let greenIndexes2 = g2 |> map (\(_, _, x) -> x)
    let totalGreenIndexes = greenIndexes1 ++ greenIndexes2
    u |> filter (`elem` totalGreenIndexes)


step :: String -> State -> State
step command state = do
    let new_state = command |> parse
    let state2 = updateState state new_state
                 |> generateWords
                 |> filterWords
    State
        (wordList state2)
        (greens state2)
        (yellows state2)
        (bads state2)
        (unknowns state2)
        (board state2)

fullStep :: IO State
fullStep = do
    command <- getLine
    let emptyState = State [] [] [] [] [] []
    print "Working..."
    let state = step command emptyState
    print $ fancyPrint state
    return state

fancyPrint :: State -> String
fancyPrint state = printf "Total words: %d" (length (wordList state))

main :: IO ()
main = do
    state <- fullStep
    main

--     if state.words |> tuple |> len == 0:
--         case cheat_level:   
--             match 0:
--                 "generating words sans bad letters..." |> print
--                 # start_time = time.perf_counter()
--                 words = generate_words(state.bads)
--                 # end_time = time.perf_counter()
--                 # "finished in: " + (end_time - start_time |> str) |> print
--                 'total words: ' + (words |> len |> str) + '\n' |> print
--             match 1:
--                 words = wordle_words.all_words
--             match 2:
--                 words = wordle_words.possible_solutions
--     else:
--         words = state.words


-- def fancy_print(state: State, word_max: int = 1000):
--     words = state.words |> fancy_format
--     if state.words |> len > word_max:
--         print("Too many words to print.")
--     else:
--         print(words)
--     print("Total words: " + str(len(state.words)))
--     print()
--     print("Board: \n" + color_board(state.board))

-- def color_letter(letter: Letter) -> str:
--     case letter[1]:
--         match 'g':
--             ouput = f"{bcolors.GREEN}{letter[0]}{bcolors.ENDC}"
--         match 'y':
--             output = f"{bcolors.YELLOW}{letter[0]}{bcolors.ENDC}"
--         match 'b':
--             output = f"{bcolors.BLACK}{letter[0]}{bcolors.ENDC}"
--     return output

-- color_board = fmap$(fmap$(color_letter) ..> join $ ('')) ..> join $ ('\n')

-- def main():
--     state = State()
--     # thing.strip() |> print
--     while True:
--         print()
--         i = input("yes?: ")
--         state = (i, state) |*> step$(cheat_level=1)
--         state |> fancy_print

-- def color_green(text) = f"{bcolors.GREEN}{text}{bcolors.ENDC}"

-- def color_yellow(text) = f"{bcolors.YELLOW}{text}{bcolors.ENDC}"

-- def color_black(text) = f"{bcolors.BLACK}{text}{bcolors.ENDC}"

-- class bcolors:
--     GREEN = '\033[97;42m'
--     YELLOW = '\033[97;43m'
--     BLACK = '\033[97;100m'
--     ENDC = '\033[0m'