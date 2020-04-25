import Debug.Trace

debug = flip trace

-- Given utility functions

mmap :: (a -> b) -> Maybe a -> Maybe b
mmap f  Nothing  = Nothing
mmap f (Just x)  = Just (f x)


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
    | p == wc = if(singleWildCardMatch (p:pp) (s:ss) /= Nothing) then (singleWildCardMatch (p:pp) (s:ss)) else (longerWildCardMatch (p:pp) (s:ss))
    | p == s = match wc pp ss
    | otherwise = Nothing

singleWildCardMatch (wc:pp) (s:ss)
    | pp == ss = Just [s]
    | otherwise = Nothing

longerWildCardMatch (wc:pp) (s:ss) = (mmap (s:) (match wc (wc:pp) ss)) --`debug` (show ("matching '" ++ (wc:pp) ++ "' and '" ++ ss ++ "'"))