type WordleLetter = (Char, Int)

main = do
    let x = generateWords "abcdef"
    print x

(|>) :: a -> (a -> b) -> b
(|>) x f = f x
infixl 1 |>

(>>>) :: (a -> b) -> (b -> c) -> a -> c
(>>>) = flip (.)

(|*>) x f = x |> uncurry f
infixl 1 |*>

generateWords :: String -> [String]
generateWords bads = do
    let alphabet = "abcdefghijklmnopqrstuvwxyz"
    let chars = alphabet |> filter (`notElem` bads)
    sequence [chars, chars, chars, chars, chars]

fitsAllGreens :: String -> [WordleLetter] -> Bool
fitsAllGreens = all . atIndex

-- def filter_words(words, greens, yellows, unknown, bads) =
--     words \ 
--     |> fmap$(tuple) \ # this is needed for test parity
--     |> filter$(check_word$(greens, yellows, unknown, bads)) \
--     |> tuple

fitsYellow :: String -> WordleLetter -> [Int] -> Bool
fitsYellow word (char, pos) unknowns =
    not (atIndex word (char, pos)) && atAnyIndex (word, char) unknowns

uAtIndex :: String -> Char -> Int -> Bool
uAtIndex = curry . atIndex

atAnyIndex :: (String, Char) -> [Int] -> Bool
atAnyIndex = any . uncurry (curry . atIndex)

atIndex :: String -> WordleLetter -> Bool
atIndex word (c, pos) = word !! pos == c

-- atAnyIndex :: Eq a => a -> [a] -> [Int] -> Bool
-- atAnyIndex x ys poses = poses |> map (atIndex x ys) |> all (== True)

-- atAnyIndex :: Eq a => a -> [a] -> [Int] -> Bool
-- atAnyIndex unknown word yellow= unknown |> (atIndex word )

-- def fits_yellow(word, unknown, yellow):
--     if word[yellow[1]] != yellow[0]:
--         res = False
--         for u in unknown:
--             if word[u] == yellow[0]:
--                 return True
--     return False

-- def fits_all_yellows(yellows, unknown, word) -> int:
--     for yellow in yellows:
--         if not fits_yellow(word, unknown, yellow):
--             return False
--     return True

-- def bad_letters_in_word(bads, word):
--     for bad in bads:
--         if bad in word:
--             return True
--     return False

-- def check_word(greens, yellows, unknown, bads, word):
--     if not word |> bad_letters_in_word$(bads):
--         if word |> fits_all_greens$(greens):
--             if word |> fits_all_yellows$(yellows, unknown):
--                 # if ''.join(word) in words.all_words:
--                 return True
--     return False