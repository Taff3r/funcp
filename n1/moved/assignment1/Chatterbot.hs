module Chatterbot where
import Utilities
import System.Random
import Data.Char
import Data.Maybe

chatterbot :: String -> [(String, [String])] -> IO ()
chatterbot botName botRules = do
    putStrLn ("\n\nHi! I am " ++ botName ++ ". How are you?")
    botloop
  where
    brain = rulesCompile botRules
    botloop = do
      putStr "\n: "
      question <- getLine
      answer <- stateOfMind brain
      putStrLn (botName ++ ": " ++ (present . answer . prepare) question)
      if (not . endOfDialog) question then botloop else return ()

--------------------------------------------------------

type Phrase = [String]
type PhrasePair = (Phrase, Phrase)
type BotBrain = [(Phrase, [Phrase])]


--------------------------------------------------------
-- Map the function (map2 (id, pick r) (tuple)) over the entire brain. Return curried function rulesApply waiting for phrase input.
stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
stateOfMind brain = 
    do
        r <- randomIO :: IO Float
        return (rulesApply ((map . map2) (id, pick r) brain)) -- Return function waiting for Phrase input


rulesApply :: [PhrasePair] -> Phrase -> Phrase
rulesApply ps phr
            | isNothing applied = []
            | otherwise = (\(Just p) -> p) applied
            where applied = transformationsApply "*" reflect ps phr

reflect :: Phrase -> Phrase
reflect [] = []
reflect (w:ws) 
            | null found  = w : reflect ws
            | otherwise   = snd (head found) : reflect ws
            where found   = filter (\(a, b) -> a==w) reflections
reflections =
  [ ("am",     "are"),
    ("was",    "were"),
    ("i",      "you"),
    ("i'm",    "you are"),
    ("i'd",    "you would"),
    ("i've",   "you have"),
    ("i'll",   "you will"),
    ("my",     "your"),
    ("me",     "you"),
    ("are",    "am"),
    ("you're", "i am"),
    ("you've", "i have"),
    ("you'll", "i will"),
    ("your",   "my"),
    ("yours",  "mine"),
    ("you",    "me")
  ]


---------------------------------------------------------------------------------

endOfDialog :: String -> Bool
endOfDialog = (=="quit") . map toLower

present :: Phrase -> String
present = unwords

prepare :: String -> Phrase
prepare = reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|") 

rulesCompile :: [(String, [String])] -> BotBrain
rulesCompile [] = []
rulesCompile ((k, a):ts) = map2 (words . map toLower, map words) (k, a) : rulesCompile ts


--------------------------------------


reductions :: [PhrasePair]
reductions = (map.map2) (words, words)
  [ ( "please *", "*" ),
    ( "can you *", "*" ),
    ( "could you *", "*" ),
    ( "tell me if you are *", "are you *" ),
    ( "tell me who * is", "who is *" ),
    ( "tell me what * is", "what is *" ),
    ( "do you know who * is", "who is *" ),
    ( "do you know what * is", "what is *" ),
    ( "are you very *", "are you *" ),
    ( "i am very *", "i am *" ),
    ( "hi *", "hello *")
  ]

reduce :: Phrase -> Phrase
reduce = reductionsApply reductions

reductionsApply :: [PhrasePair] -> Phrase -> Phrase
reductionsApply r = fix (try (transformationsApply "*" id r))


-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute _ [] _ = []
substitute wc (l:ls) sub
    | wc == l = sub ++ substitute wc ls sub
    | otherwise = l : substitute wc ls sub

-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just []
match _ [] _ = Nothing 
match _ _ [] = Nothing
match wc (p:pp) (s:ss)
    | p == wc = orElse (singleWildcardMatch (p:pp) (s:ss)) (longerWildcardMatch (p:pp) (s:ss))
    | p == s = match wc pp ss 
    | otherwise = Nothing

-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch _ [] = Nothing
singleWildcardMatch [] _ = Nothing
singleWildcardMatch (wc:pp) (s:ss) = mmap (const [s]) (match wc pp ss)

longerWildcardMatch _ [] = Nothing
longerWildcardMatch [] _ = Nothing
longerWildcardMatch (wc:pp) (s:ss) = mmap (s:) (match wc (wc:pp) ss)

-- Test cases --------------------

testPattern =  "a=*;"
testSubstitutions = "32"
testString = "a=32;"

substituteTest = substitute '*' testPattern testSubstitutions
substituteCheck = substituteTest == testString

matchTest = match '*' testPattern testString
matchCheck = matchTest == Just testSubstitutions



-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------

-- Applying a single pattern
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
transformationApply wc f s t = mmap (substitute wc sec . f) ok
    where sec = snd t
          fir = fst t
          ok  = match wc fir s


-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply _ _ [] _ = Nothing
transformationsApply wc f (p:lps) s = orElse (transformationApply wc f s p) (transformationsApply wc f lps s)

