{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Applicative ((<|>))
import qualified Data.Vector as V (Vector, replicate, map, toList)
import Data.Vector ((//), (!))

import Data.Attoparsec.Text (
    Parser,
    char,
    decimal,
    endOfLine,
    letter,
    many1,
    parse,
    parseOnly,
    sepBy,
    string)

problemInput = do
    stateLines <- many1 stateLine
    stateFooter
    endOfLine
    instructions <- many1 instruction
    return (stateLines, instructions)

stateLine = do
    items <- stateItem `sepBy` (char ' ')
    endOfLine
    return items

stateItem = emptyBlock <|> itemBlock

emptyBlock = do
    string "   "
    return ' '

itemBlock = do
    char '['
    item <- letter
    char ']'
    return item

newline = char '\n'

stateFooter = do
    (footerItem `sepBy` (char ' '))
    endOfLine
    return ()

footerItem = do
    char ' '
    x <- decimal
    char ' '
    return x

instruction = do
    string "move "
    n <- decimal
    string " from "
    src <- decimal
    string " to "
    dst <- decimal
    endOfLine
    return (n, src, dst)

-----------------------
type S = V.Vector [Char]

_take :: S -> Int -> (S, Char)
_take state n = (state', c) where
    index = n - 1
    state' = state // [(index, stack')]
    stack = state ! index
    c = head stack
    stack' = tail stack

_put :: S -> Int -> Char -> S
_put state n c = state' where
    index = n - 1
    state' = state // [(index, stack')]
    stack = state ! index
    stack' = c : stack

initShip :: [[Char]] -> S
initShip stateLines = 
    foldl
        (\state (n, c) -> _put state n c)
        (V.replicate (length . head $ stateLines) [])
    $ filter (\(n,c)->c/=' ') 
    $ concat 
    $ map (\list->zip [1..] list) 
    $ reverse
    $ stateLines

part1 (Right (stateLines, instructions)) = getTopmost finalState where
    finalState = foldl execInstruction initialState instructions
    initialState = initShip stateLines

execInstruction :: S -> (Int, Int, Int) -> S
execInstruction state (0, _, _) = state
execInstruction state (n, from, to) = execInstruction state'' ((n-1), from, to) where
    state'' = _put state' to c
    (state', c) = _take state from

getTopmost :: S -> [Char]
getTopmost s = V.toList $ V.map head $ s

part2 (Right (stateLines, instructions)) = getTopmost finalState where
    finalState = foldl execInstruction9001 initialState instructions
    initialState = initShip stateLines

execInstruction9001 :: S -> (Int, Int, Int) -> S
execInstruction9001 state (n, from, to) = state'' where
    state'' = _putN state' to cl
    (state', cl) = _takeN state from n

_putN state to cl = state' where
    index = to - 1
    state' = state // [(index, stack')]
    stack' = cl ++ stack
    stack = state ! index

_takeN state from n = (state', cl) where
    index = from - 1
    state' = state // [(index, stack')]
    stack' = drop n stack
    cl = take n stack
    stack = state ! index

main :: IO ()
main = do
    example <- TIO.readFile "05-example.txt"
    putStrLn . show $ part1 $ parseOnly problemInput $ example
    input <- TIO.readFile "../data/05.txt"
    putStrLn . show $ part1 $ parseOnly problemInput $ input
    putStrLn . show $ part2 $ parseOnly problemInput $ example
    putStrLn . show $ part2 $ parseOnly problemInput $ input
