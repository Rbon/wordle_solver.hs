import LibRbon

type WordleLetter = (Char, Int)

main = do
    let x = generateWords "abcdef"
    print x

generateWords :: String -> [String]
generateWords bads = do
    let alphabet = "abcdefghijklmnopqrstuvwxyz"
    let chars = alphabet |> filter (`notElem` bads)
    sequence [chars, chars, chars, chars, chars]

fitsAllGreens :: String -> [WordleLetter] -> Bool
fitsAllGreens = atIndex |-> uncurry |-> all

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

filterWords :: [WordleLetter]
            -> [WordleLetter]
            -> [Int]
            -> [Char]
            -> [String]
            -> [String]
filterWords greens yellows unknown bads =
    filter (checkWord greens yellows unknown bads)



pairs :: String -> [(Char, Char)]
pairs = splitOnString " = " |-> (\[x, y] -> (x,y)) |=> zip



-- def parse(command: str) -> State:
--     pairs = command |> split $ (" = ") |*> zip
--     greens = []
--     yellows = []
--     bads = []
--     unknown = [0,1,2,3,4]
--     index = 0
--     for i in pairs:
--         if i[1] == 'g':
--             greens.append((i[0], index))
--             if index in unknown:
--                 unknown.remove(index) 
--         elif i[1] == 'y':
--             yellows.append((i[0], index))
--         elif i[1] == 'b':
--             bads.append((i[0]))
--         index += 1
--     print 
--     return State((), greens, yellows, bads, unknown, [pairs |> tuple])

-- def add_state(state1: State, state2: State) -> State:
--     words = state1.words :: state2.words
--     greens = state1.greens :: state2.greens
--     yellows = state1.yellows :: state2.yellows
--     bads = state1.bads :: state2.bads
--     unknown = state1.unknown :: state2.unknown
--     board = state1.board + state2.board
--     return State(words, greens, yellows, bads, unknown, board)

-- def step(
--     command: str,
--     state: State = State(),
--     cheat_level: int = 1
--     ) -> State:

--     new_state = command |> parse
--     state = add_state(state, new_state)

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

--     "filtering words..." |> print
--     # start_time = time.perf_counter()
--     words = filter_words(words, state.greens, state.yellows, state.unknown, state.bads)
--     # end_time = time.perf_counter()
--     # "finished in: " + (end_time - start_time |> str) |> print
--     print("total words: " + str(len(words)) + '\n')

--     return State(
--         words,
--         state.greens,
--         state.yellows,
--         state.bads,
--         state.unknown,
--         state.board)


-- def count_vowels(word) = word |> filter $ (x -> x in 'aeiouy') |> tuple |> len

-- def reveal_4_vowel_words(word):
--     if (word |> count_vowels) == 4:
--         if not (len(word) != len(set(word))): # filter dupes
--             print word

-- def reveal_5_vowel_words(word):
--     if (word |> count_vowels) == 5:
--         if not (len(word) != len(set(word))): # filter dupes
--             print word       

-- def fancy_format(iterable) = '\n'.join(tuple(map(''.join, iterable)))

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