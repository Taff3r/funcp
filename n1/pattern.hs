import Data.List
substitute :: Eq a => a -> [a] -> [a] -> [a] 
substitute _ [] _  = []
substitute wc (t:ts) s
          | wc == t = s ++ (substitute wc ts s)
          | otherwise = t : substitute wc ts s

match :: Eq a => a -> [a] -> [a] ->  [a]
match wc p  [] = [] 
match wc [] s  = []
match wc (p:pp) (s:ss)  
            | s:ss == p:pp = []
            | p == wc = takeUntil pp (s:ss)
            | otherwise = match wc pp ss

-- singleWildcardMatch :: Eq a => a -> [a] -> [a] -> Maybe [a]
-- longerWildcardMatch :: Eq a => a -> [a] -> [a] -> Maybe [a]

takeUntil :: Eq a => [a] -> [a] -> [a]
takeUntil _ [] = []
takeUntil a b
            | a == b = [] 
            | otherwise = head b : takeUntil a (tail b)


