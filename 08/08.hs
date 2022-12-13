import Data.List (lines, transpose, concat, intercalate, sort)
import Data.Char (ord)
import Data.Maybe (listToMaybe)

-- parse input into a height matrix with visibility flags
initVisibility :: [String] -> [[(Bool, Int)]]
initVisibility = map $ zip (repeat False) . map (\c -> ord(c) - ord('0')) 

-- update visibility flags when looking at the row from left (head) to right (tail)
updateVisibilityRow :: [(Bool, Int)] -> [(Bool, Int)]
updateVisibilityRow list = reverse list' where
    (list', _) = foldl f ([], -1) list
    f (l, threshold) (visible, height) =
        ((visible || height > threshold, height):l, max height threshold)

-- update visibility flags across the matrix
updateVisibility1 :: [[(Bool, Int)]] -> [[(Bool, Int)]]
updateVisibility1 = map updateVisibilityRow

-- update visibility flags four times, looking from four directions
updateVisibility :: [[(Bool, Int)]] -> [[(Bool, Int)]]
updateVisibility m =
      updateVisibility1
    $ map reverse
    $ updateVisibility1
    $ transpose
    $ map reverse
    $ updateVisibility1
    $ map reverse
    $ updateVisibility1 m

-- count visible trees across the matrix
countVisible :: [[(Bool, Int)]] -> Int
countVisible l = length $ filter fst $ concat $ l

part1 input = 
    countVisible
    $ updateVisibility
    $ initVisibility 
    $ lines input

--- Part Two ---
-- Visible profile -- positions and heights of trees visible towards left (head) of a tree row
type Profile = [(Int, Int)] -- list of height, pos pairs

-- Useful predicates: profile elements below/above given height
below :: Int -> (Int, Int) -> Bool
below height (profHeight, _) = profHeight < height
above :: Int -> (Int, Int) -> Bool
above height (profHeight, _) = profHeight > height

-- Find intersection of visible profile at a given height
-- Nothing would mean visibility is not blocked at the given height
-- Just (position, height) means visibility is blocked by a tree at returned position 
profileIntersection :: Profile -> Int -> Maybe (Int, Int) 
profileIntersection prof height = listToMaybe $ filter (not . below height) prof

-- Update profile with newtree: all trees below new tree's height are obscured and removed
-- Important invariant: profile is always sorted by heights ascending,
-- as lower trees are added to the head and higher trees are kept in the tail
profileUpdate :: Profile -> (Int, Int) -> Profile
profileUpdate prof pair@(height, pos) = pair:(filter (above height) prof)

-- An experimental testbed to look at visible profile running along a row of trees
-- intermediate profiles, intersections and visibility distance are recorded
data Record = R { el :: (Int, Int), vd::Int, origProf :: Profile, newProf :: Profile, inter :: Maybe (Int, Int)} deriving Show
run :: [Int] -> [Record]
run heights = snd $ foldl f ([], []) $ zip heights [0..] where
    f (profile, history) element@(height, pos) = (profile', record:history) where
        profile' = profileUpdate profile element
        inter = profileIntersection profile height
        vd = case inter of
            Nothing -> pos
            Just (profHeight, profPos) -> pos - profPos
        record = R { origProf=profile, vd=vd, newProf=profile', el=element, inter=inter}
experiment heights = do
    putStrLn $ show $ heights
    putStrLn $ intercalate "\n" $ map show $ reverse $ run heights

-- Parse input into matrix of heights and visibility scores (inital scores are 1)
initScenic :: [[Char]] -> [[(Int, Int)]]
initScenic = map $ zip (repeat 1) . map (\c -> ord(c) - ord('0')) 

-- Run through a row from left (head) to the right (tail)
-- keeping track of visibility distance towards it's left end (list head).
-- Scenic score is updated for each tree
updateScenicRow :: [(Int, Int)] -> [(Int, Int)]
updateScenicRow list = reverse list' where
    (list', _) = foldl f ([], []) (zip [0..] list)
    f state@(result, profile) elem@(pos, (score, height)) =
        ((score',height):result, profile') where
            score' = score * vd
            vd = case profileIntersection profile height of
                Nothing -> pos
                Just (profHeight, profPos) -> pos - profPos
            profile' = profileUpdate profile (height, pos)

-- update scenic scores across the matrix
updateScenic1 :: [[(Int, Int)]] -> [[(Int, Int)]]
updateScenic1 = map updateScenicRow

-- update scenic scores for four directions
updateScenic :: [[(Int, Int)]] -> [[(Int, Int)]]
updateScenic m =
      updateScenic1
    $ map reverse
    $ updateScenic1
    $ transpose
    $ map reverse
    $ updateScenic1
    $ map reverse
    $ updateScenic1 m

-- find the max score
maxScore :: [[(Int, a)]] -> Int
maxScore l = foldl max 0 $ map fst $ concat $ l

part2 input = 
    maxScore
    $ updateScenic
    $ initScenic 
    $ lines input

main :: IO ()
main = do
    example <- readFile "08-example.txt"
    input <- readFile "../data/08.txt"
    putStrLn "part 1"
    putStrLn $ show $ part1 $ example
    putStrLn $ show $ part1 $ input
    putStrLn "part 2"
    putStrLn $ show $ part2 $ example
    putStrLn $ show $ part2 $ input
