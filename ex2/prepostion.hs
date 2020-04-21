import Data.List
data Shape = Circle Float Float Float
           | Rectangle Float Float Float Float
           | Square Float
    deriving(Show)

data Proposition = Var Name 
                 | Proposition :&: Proposition
                 | Proposition :|: Proposition
                 | Not Proposition
    deriving( Show, Eq , Read) 
type Name = String

area :: Shape -> Float
area (Circle _ _ r) = pi * r * 2
area (Rectangle x1 x2 y1 y2) = (abs x1 - x2) * (abs y1 - y2)
area (Square x) = x * x

isRight::(Int, Int, Int) -> Bool
isRight (s1, s2, h) = s1 * s1 + s2 * s2 == h * h

findRights:: [(Int, Int, Int)] -> [(Int, Int, Int, Bool)]
findRights [] = []
findRights ((s1, s2, h):xs) = [(s1, s2, h, t)] ++ findRights xs
                           where t = isRight (s1, s2, h)

myTake::Int -> [a] -> [a]
myTake _ [] = []
myTake 0 _ = []
myTake n (x:xs) = x : myTake (n-1) xs

tokenize::[Char] -> [[Char]]
tokenize "" = []
tokenize s = head : tokenize tail
           where head = takeWhile (\c -> c /= ' ') s 
                 tail = drop ((length head) + 1) s

flatten::[a] -> a
flatten [] = undefined 
flatten [a] = a

flatten2d::[[a]] -> [a]
flatten2d (as) = [flatten a | a <- as]
