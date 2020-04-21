-- 1
myLast::[a] -> a
myLast [] = undefined 
myLast [a] = a
myLast (a:as) = myLast as
-- 2
myButLast:: [a] -> a
myButLast (a:as)  
    | len > 1 = myButLast as
    | otherwise = a
    where len = length as

-- 3
elementAt::[a] -> Int -> a
elementAt as n = as !! (n - 1)

-- 4
myLength:: [a] -> Int
myLength xs = length xs

-- 5
myReverse::[a] -> [a]
myReverse [a] = [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]
