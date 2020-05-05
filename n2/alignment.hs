import Data.List

 -- Code by Simon Tenggren (si6187te-s) 
 -- &&      Elias Rudberg  (TODO: ELIAS)

scoreMatch    =  0 -- Or should it be 1?
scoreMismatch = -1
scoreSpace    = -1 -- Or should it be -2?

type AlignmentType = (String, String)

-- Assume the strings have already been optimaly aligned.
similiarityScore :: String -> String -> Int
similiarityScore _ [] = 0
similiarityScore [] _ = 0
similiarityScore (s:ss) (p:ps)
        | s == '-' || p == '-' = scoreSpace + similiarityScore ss ps
        | s == p               = scoreMatch + similiarityScore ss ps
        | otherwise            = scoreMismatch + similiarityScore ss ps

-- Attach heads adds h1 and h2 as heads to all lists in the list of tuples of lists (aList)
-- Uses list comprehension instead of recursion
-- attachHeads 0 1 [([2,2,2], [3,3,3])] = [([0,2,2,2], [1,3,3,3])]
attachHeads :: a -> a -> [([a], [a])] -> [([a], [a])]
attachHeads h1 h2 aList = [(h1:xs, h2:ys) | (xs, ys) <- aList]


-- Returns the elements which score the highest comparable score b from the function valueFcn
-- e.g. maximaBy length ["cs", "ow", "o"] = ["cs", "ow"]
maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFcn xs = [x | x <- sorted, valueFcn x == score]
                     -- Reverse sort to get largest/highest score first
                     where sorted = sortBy (\x y -> compare (valueFcn y) (valueFcn x)) xs 
                           score  = valueFcn (head sorted)

optAlignments :: String -> String -> [AlignmentType]
optAlignments (s:ss)  [] = attachHeads s '-' (optAlignments ss []) 
optAlignments [] (p:ps)  = attachHeads '-' p (optAlignments [] ps)   
optAlignments [] []      = [("","")]
optAlignments (s:ss) (p:ps) = maximaBy (uncurry similiarityScore) branches
                           where branches = branch1 ++ branch2 ++ branch3
                                 branch1 = attachHeads '-' p (optAlignments (s:ss) ps)
                                 branch2 = attachHeads s '-' (optAlignments ss (p:ps))
                                 branch3 = attachHeads s p (optAlignments ss ps)


outputOptAlignments p s = showAlignments len alignments
                        where alignments = snd $ tableAlignment p s
                              len        = length alignments

showAlignments :: Int -> [(String, String)] -> IO()
showAlignments n al = putStrLn $ formatAlignments al ++ "There are " ++ show n ++ " optimal alignment(s)!"

formatAlignments :: [AlignmentType] -> String
formatAlignments = concatMap (\(s,p) -> formatString s ++ "\n" ++ formatString p ++ "\n\n")

formatString :: String -> String
formatString "" = ""
formatString (s:ss) = s : " " ++ formatString ss

tableAlignment :: String -> String -> (Int, [AlignmentType])
tableAlignment s p = (score, revl) 
    where
       revl       = map (\(a, b) -> (reverse a, reverse b)) l
       (score, l) = alignmentScore (length s) (length p)
       alignmentScore :: Int -> Int -> (Int, [AlignmentType])
       alignmentScore i j = alignmentTable !! i !! j

       alignmentTable :: [[(Int, [AlignmentType])]] 
       alignmentTable = [[alignmentEntry i j | j <- [0..]] | i <- [0..]]

       alignmentEntry :: Int -> Int -> (Int, [AlignmentType])
       -- base cases
       alignmentEntry 0 0 = (0, [("" , "")])
       alignmentEntry i 0 = (i * scoreSpace, [(take i s, replicate i '-')])
       alignmentEntry 0 j = (j * scoreSpace, [(replicate j '-', take j s)])
       alignmentEntry i j
            | x == y = (scoreMatch + s3 , attachHeads x y a3)
            | otherwise = (fst (head list), concatMap snd list)
                    where 
                         list     = maximaBy fst [(s1 + scoreSpace, branch1),
                                                  (s2 + scoreSpace, branch2),
                                                  (s3 + scoreMismatch, branch3)] 
                         (s1, a1) = alignmentScore i (j - 1)
                         (s2, a2) = alignmentScore (i - 1) j
                         (s3, a3) = alignmentScore (i - 1) (j - 1)
                         branch1  = attachHeads '-' y  a1
                         branch2  = attachHeads x '-'  a2 
                         branch3  = attachHeads x y    a3 
                         
                         x = s !! (i-1)
                         y = p !! (j-1)
     
tableScore :: String -> String -> Int
tableScore s p = score (length s) (length p)
  where
    score i j = scoreTable !!i!!j
    scoreTable :: [[Int]]
    scoreTable = [[ scoreEntry j i | i <-[0..]] | j <-[0..] ]
    scoreEntry :: Int -> Int -> Int
    -- base cases
    scoreEntry 0 0 = 0              -- Score 0 when length of both strings are 0
    scoreEntry i 0 = scoreSpace * i -- Score space * i when right hand string is empty
    scoreEntry 0 j = scoreSpace * j -- Score space * j when left hand string is empty 
    scoreEntry i j 
        | x == '-' || y == '-' = scoreSpace    + max (score i (j - 1)) (score (i - 1) j)
        | x == y               = scoreMatch    + score (i - 1) (j - 1)
        | otherwise            = scoreMismatch + score (i - 1) (j - 1)
      where
         x = s !! (i-1)
         y = p !! (j-1)
