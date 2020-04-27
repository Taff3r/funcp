import Data.List
type Phrase     = [String]
type PhrasePair = (Phrase, Phrase)
type BotBrain   = [(Phrase, [Phrase])]

reflections =
  [("am", "are"),
    ("was", "were"),
    ("i", "you"),
    ("i'm", "you are"),
    ("i'd", "you would"),
    ("i've", "you have"),
    ("i'll", "you will"),
    ("my", "your"),
    ("me", "you"),
    ("are", "am"),
    ("you're", "i am"),
    ("you've", "i have"),
    ("you'll", "i will"),
    ("your", "my"),
    ("yours", "mine"),
    ("you", "me")
  ]
reflect :: Phrase -> Phrase
reflect [] = []
reflect (w:ws) 
            | found == [] = [w] ++ reflect ws
            | otherwise   = (snd (head found)) : reflect ws
            where found   = filter (\(a, b) -> a==w) $ reflections
