import System.Random
import Utilities

type Phrase = [String]
type PhrasePair = (Phrase, Phrase)
type BotBrain = [(Phrase, [Phrase])]

testR = 
    do
        r <- randomIO :: IO Float
        return map (pick r [("hello", ["there", "general", "kenboi"]), ("asdf", ["egasdg", "asdf"])])
