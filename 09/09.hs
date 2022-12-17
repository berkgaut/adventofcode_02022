{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Set as S (fromList, size)
import Control.Applicative ((<|>))
import Data.List(takeWhile, iterate, intercalate)

import Data.Attoparsec.Text (
    Parser,
    char,
    decimal,
    endOfLine,
    letter,
    many1,
    parse,
    parseOnly,
    sepBy, space,
    string, satisfy, inClass)
    
import Data.List (singleton, concatMap)
import Data.Char (ord)
import Data.Maybe (listToMaybe)

--- Parse Input ---

data Dir = U | D | L | R deriving (Read, Show, Eq)

direction = satisfy (inClass "UDLR") >>= return . read . singleton

line :: Parser (Dir, Int)
line = do
    dir <- direction
    space
    steps <- decimal
    return (dir, steps)

--- Part 1 ---

simplify :: [(Dir, Int)] -> [Dir]
simplify = concatMap (\(dir, n)->take n $ repeat dir) 

delta :: Maybe Dir -> (Int, Int)
delta dir = case dir of
    Nothing  -> ( 0,  0)
    (Just U) -> ( 0,  1)
    (Just D) -> ( 0, -1)
    (Just L) -> (-1,  0)
    (Just R) -> ( 1,  0)

update pos@(x, y) delta@(dx, dy) = (x + dx, y + dy)

updateTail tailPos@(tailX, tailY) headPos@(headX, headY) = update tailPos tailDelta where
    tailDelta | (abs (tailX - headX)) < 2 && (abs (tailY - headY) < 2) = (0, 0)
              | otherwise = (tailX `following` headX, tailY `following` headY)
    following tl hd = signum (hd - tl)

data State = State {
    moves :: [Dir],
    headPos :: (Int, Int),
    tailPos :: (Int, Int),
    tailPositions :: [(Int, Int)],
    stop :: Bool
    } deriving Show

initialState moves = State { moves = moves, headPos = (0, 0), tailPos = (0, 0), tailPositions = [], stop = False }
step :: State -> State
step state@State{} =
    State {moves=moves', headPos=headPos', tailPos=tailPos', tailPositions=tailPositions', stop = stop } where
        (nextMove, moves') = case moves state of
            [] -> (Nothing, [])
            (hd:tl) -> (Just hd, tl)
        headPos' = (headPos state) `update` (delta nextMove)
        tailPos' = (tailPos state) `updateTail` headPos'
        tailPositions' = tailPos' : (tailPositions state)
        stop = (headPos state == headPos') && (tailPos' == (tailPos state))

simulate moves = takeWhile (not . stop) $ iterate step $ initialState moves


part1 lines = 
    S.size 
    $ S.fromList
    $ tailPositions
    $ last
    $ simulate moves where moves = simplify $ map (\(Right x)->x) $ map (parseOnly line) $ lines

debug lines = simulate $ simplify $ map (\(Right x)->x) $ map (parseOnly line) $ lines

main :: IO ()
main = do
    example <- TIO.readFile "09-example.txt"
    input <- TIO.readFile "../data/09.txt"
    --putStrLn  $ intercalate "\n" $ map show $ debug $ T.lines $ example
    putStrLn $ show $ part1 $ T.lines $ example
    putStrLn $ show $ part1 $ T.lines $ input