{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Applicative ((<|>))
import Data.List(takeWhile, iterate, intercalate)
import Data.List.Split (chunksOf)

import Data.Attoparsec.Text (
    Parser,
    char,
    decimal,
    endOfLine,
    letter,
    many1,
    signed,
    parse,
    parseOnly,
    sepBy, space,
    string, satisfy, inClass)
    
import Data.List (singleton, concatMap)
import Data.Char (ord)
import Data.Maybe (listToMaybe)

--- Parse Input ---

data Insn = Noop | Addx Int deriving Show

inputLine = noop <|> addx
noop = string "noop" >> return Noop
addx = string "addx" >> char ' ' >> signed decimal >>= (return . Addx)

parseInput lines = map (\(Right x)->x) $ map (parseOnly inputLine) $ lines

-- Interpret input

data State = State { x :: Int, history :: [Int] }

step :: State -> Insn -> State
step (State x history) Noop     = State x     (x:history)
step (State x history) (Addx n) = State (x+n) ([x,x] ++ history)

run :: [Insn] -> [(Int, Int)]
run insns =
    zip [1..] -- cycle count
    $ reverse
    $ history
    $ foldl step (State 1 [])
    $ insns

--- Part 1 ---

part1 lines = 
    sum
    $ map (\(cycle, x)->cycle*x)
    $ filter (\(cycle, x)-> cycle `elem` [20, 60, 100, 140, 180, 220])
    $ run $ parseInput $ lines

--- Part 2 ---

pixel :: (Int, Int) -> Char
pixel (cycle, x) = repr where
    repr = if visible then '#' else ' '
    visible = (abs (rayPos - spritePos)) <= 1
    spritePos = x
    rayPos = (cycle - 1) `mod` 40

screen :: [(Int, Int)] -> String
screen cycles = 
    intercalate "\n"
    $ chunksOf 40
    $ map pixel
    $ cycles

part2 lines = 
    screen
    $ run $ parseInput $ lines

main :: IO ()
main = do
    example <- TIO.readFile "10-example.txt"
    input <- TIO.readFile "../data/10.txt"
    putStrLn "--- Part 1 ---"
    putStrLn $ show $ part1 $ T.lines $ example
    putStrLn $ show $ part1 $ T.lines $ input
    putStrLn "--- Part 2 ---"
    putStrLn "----------------------------------------"
    putStrLn $ part2 $ T.lines $ example
    putStrLn "----------------------------------------"
    putStrLn $ part2 $ T.lines $ input
