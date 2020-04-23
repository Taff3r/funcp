import Data.List
substitute :: Eq a => a -> [a] -> [a] -> [a] 
substitute _ [] _  = []
substitute wc (t:ts) s
          | wc == t = s ++ (substitute wc ts s)
          | otherwise = t : substitute wc ts s

match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match wc p  [] = Nothing 
match wc [] s  = Nothing 

match wc (p:pp) s  
            | p == wc = if (pp == tail (s))
                            then singleWildcardMatch pp s
                        else
                            longerWildcardMatch (p:pp) s
            | s == p:pp = Just []
            | otherwise = match wc pp (tail s)

singleWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch [] _ = Nothing
singleWildcardMatch _ [] = Nothing
singleWildcardMatch (p:ps) (s:ss)
            | match p (p:ps) (s:ss) == Nothing = Just [s] 
            | otherwise = Nothing
                    
longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
longerWildcardMatch _ [] = Nothing
longerWildcardMatch [] _ = Nothing
longerWildcardMatch (p:ps) (s:ss) 
    | match p (p:ps) (s:ss) == Nothing =  Just (maybe [] (s :) (match p (p:ps) ss))
    | otherwise = Just (maybe [] (s :) (match p (p:ps) ss))

