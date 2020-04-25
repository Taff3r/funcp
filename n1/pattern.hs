import Debug.Trace

debug = flip trace

-- Given utility functions

mmap :: (a -> b) -> Maybe a -> Maybe b
mmap f  Nothing  = Nothing
mmap f (Just x)  = Just (f x)

orElse :: Maybe a -> Maybe a -> Maybe a
orElse Nothing x = x
orElse (Just a) _ = Just a

try :: (a -> Maybe a) -> a -> a
try f x = maybe x id (f x)

-- Own implemented functions
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute _ [] _ = []
substitute wc (l:ls) sub
    | wc == l = sub ++ (substitute wc ls sub)
    | otherwise = l : (substitute wc ls sub)

-- match :: Eq a => a -> [a] -> [a] -> Maybe [a] (Type will make debug print error)
match _ [] [] = Just []
match _ [] _ = Nothing `debug` "match got empty p"
match _ _ [] = Nothing `debug` "match got empty s"
match wc (p:pp) (s:ss)
    | p == wc = orElse (singleWildcardMatch (p:pp) (s:ss)) (longerWildcardMatch (p:pp) (s:ss))
    | p == s = match wc pp ss `debug` "trimming"
    | otherwise = Nothing

singleWildcardMatch _ [] = Nothing
singleWildcardMatch [] _ = Nothing
singleWildcardMatch (wc:pp) (s:ss) = mmap (const [s]) (match wc pp ss)

longerWildcardMatch _ [] = Nothing
longerWildcardMatch [] _ = Nothing
longerWildcardMatch (wc:pp) (s:ss) = (mmap (s:) (match wc (wc:pp) ss)) -- `debug` (show ("matching '" ++ (wc:pp) ++ "' and '" ++ ss ++ "'"))

