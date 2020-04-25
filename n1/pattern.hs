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

match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just []
match _ [] _ = Nothing `debug` "match got empty p"
match _ _ [] = Nothing `debug` "match got empty s"

match wc (p:pp) (s:ss)
    | p == wc = if(singleWildcardMatch (p:pp) (s:ss) /= Nothing) then (singleWildcardMatch (p:pp) (s:ss)) else (longerWildcardMatch (p:pp) (s:ss))
    | p == s = match wc pp ss
    | otherwise = Nothing

singleWildcardMatch (wc:ps) (x:xs) = mmap (const [x]) (match wc ps xs)

longerWildcardMatch (wc:ps) (x:xs) = mmap (x:) (match wc (wc:ps) xs)