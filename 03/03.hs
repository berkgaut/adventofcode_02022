{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Set as S
import Data.Bits (shiftR)
import Data.Char (ord)
import Data.List.Split (chunksOf)

compartments :: T.Text -> (T.Text, T.Text)
compartments t = T.splitAt (shiftR (T.length t) 1) t

i = S.intersection -- shortcut for infix use

sft :: T.Text -> S.Set Char
sft = S.fromList . T.unpack

errors :: (T.Text, T.Text) -> [Char]
errors (a, b) = S.toList 
    $ (sft a) `i` (sft b)

prio :: Char -> Int
prio c | c >= 'a' && c <= 'z' = ord c - ord 'a' + 1
       | c >= 'A' && c <= 'Z' = ord c - ord 'A' + 27
       | otherwise = 0

part1 input = 
    sum
    $ map (sum . map prio . errors . compartments)
    $ T.lines 
    $ input

badge [elf1, elf2, elf3] = head $ S.toList $ (sft elf1) `i` (sft elf2) `i` (sft elf3)

part2 input = 
    sum
    $ map (prio . badge)
    $ chunksOf 3
    $ T.lines
    $ input

main :: IO ()
main = do
    example1 <- TIO.readFile "03-part-1-example.txt"
    putStrLn . show . part1 $ example1
    input <- TIO.readFile "../data/03.txt"
    putStrLn . show . part1 $ input
    example2 <- TIO.readFile "03-part-2-example.txt"
    putStrLn . show . part2 $ example2
    putStrLn . show . part2 $ input
