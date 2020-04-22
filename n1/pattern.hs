{-
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match wc p  [] = Nothing
match wc [] [] = []
match wc [] p  = Nothing
-}

substitute :: Eq a => a -> [a] -> [a] -> [a] 
substitute _ [] _  = []
substitute wc (t:ts) s
          | wc == t = s ++ (substitute wc ts s)
          | otherwise = t : substitute wc ts s
