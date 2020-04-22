import Data.List
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

-- 6
isPalindrome::(Eq a) => [a] -> Bool
isPalindrome ls = (ls == (myReverse ls))

-- 7
data NestedList a = Elem a | List [NestedList a]
flatten::NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List []) = []

-- 8
compress :: (Eq a) => [a] -> [a]
compress []     = []
compress [a]    = [a]
compress (a:as) 
    | elem a as = compress as
    | otherwise = [a] ++ compress as
