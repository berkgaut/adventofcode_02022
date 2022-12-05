{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Attoparsec.Text (Parser, parseOnly, char, decimal)

pairP :: Parser (Int, Int)
pairP = do
    x <- decimal
    char '-'
    y <- decimal
    return (x, y)

lineP :: Parser ((Int, Int), (Int, Int))
lineP = do
    first <- pairP
    char ','
    second <- pairP
    return (first, second)

parseLine :: T.Text -> [((Int, Int), (Int, Int))]
parseLine text = case (parseOnly lineP text) of
    Left  _error -> []
    Right result -> [result]

isPairFullyContained (a, b) (x, y) = a >= x && b <= y

isFullyContained (pair1, pair2) =
    isPairFullyContained pair1 pair2 
    || isPairFullyContained pair2 pair1

part1 input =
    length
    $ filter isFullyContained
    $ concatMap parseLine
    $ T.lines
    $ input

isOverlapping ((a, b), (x, y)) =
    ((max b y) - (min a x) + 1) < (b - a + 1) + (y - x + 1)

part2 input =
    length
    $ filter isOverlapping
    $ concatMap parseLine
    $ T.lines
    $ input

main :: IO ()
main = do
    example <- TIO.readFile "04-example.txt"
    putStrLn . show . part1 $ example
    input <- TIO.readFile "04.txt"
    putStrLn . show . part1 $ input
    putStrLn . show . part2 $ example
    putStrLn . show . part2 $ input
