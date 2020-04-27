module Pattern
       ( mmap
       , orElse
       , try
       , substitute
       , match
       , singleWildcardMatch
       , longerWildcardMatch
       , transformationApply
       , transformationsApply
       ) where

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

match :: Eq a => a -> [a] -> [a] -> Maybe [a] 
match _ [] [] = Just []
match _ [] _ = Nothing 
match _ _ [] = Nothing
match wc (p:pp) (s:ss)
    | p == wc = orElse (singleWildcardMatch (p:pp) (s:ss)) (longerWildcardMatch (p:pp) (s:ss))
    | p == s = match wc pp ss 
    | otherwise = Nothing

singleWildcardMatch _ [] = Nothing
singleWildcardMatch [] _ = Nothing
singleWildcardMatch (wc:pp) (s:ss) = mmap (const [s]) (match wc pp ss)

longerWildcardMatch _ [] = Nothing
longerWildcardMatch [] _ = Nothing
longerWildcardMatch (wc:pp) (s:ss) = (mmap (s:) (match wc (wc:pp) ss))

transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
transformationApply wc f s t = mmap (substitute wc sec . f) ok
    where sec = (snd t)
          fir = (fst t)
          ok  = match wc fir s 

transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply _ _ [] _ = Nothing
transformationsApply wc f (p:lps) s
                        | trans == Nothing = transformationsApply wc f lps s
                        | otherwise = trans
                        where trans = transformationApply wc f s p
