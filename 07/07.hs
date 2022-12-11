{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as M
import Data.List (intercalate, concat, sort)
import Control.Applicative ((<|>))

import Data.Attoparsec.Text (
    Parser,
    anyChar,
    char,
    decimal,
    endOfLine,
    letter,
    many1,
    parse,
    parseOnly,
    sepBy,
    string)

-- transcript parser

data ParsedLine = CdRoot | Cd String | CdUp | Ls | LDir String | LFile String Int
  deriving (Show)

inputLine = input <|> output
input = do
    char '$'
    char ' '
    command
command = cdroot <|> chdir <|> cdup <|> ls
cdroot = do
    string "cd /"
    return CdRoot
cdup = do
    string "cd .."
    return CdUp
chdir = do
    string "cd "
    dir <- many1 letter
    return $ Cd dir
ls = do
    string "ls"
    return Ls
output = directory <|> file
directory = do
    string "dir "
    dir <- many1 anyChar
    return $ LDir dir
file = do
    size <- decimal
    char ' '
    name <- many1 anyChar
    return $ LFile name size

--- fs model

data Node =   Dir  {_nodes :: M.Map String Node }
            | File {_size :: Int}
            deriving (Eq, Show)

isFile (name, (File _)) = True
isFile _ = False

x = Dir {_nodes=M.fromList [("a", File 12)]}

insert :: Node -> [String] -> Node -> Node
insert root@Dir{_nodes = nodes} path node = root{_nodes=nodes'} where
    nodes' = case path of
        [name] -> M.insert name node nodes
        (subdirName:restOfPath) -> M.alter (Just . f) subdirName nodes where
            f (Just existing) = insert existing      restOfPath node
            f Nothing         = insert (Dir M.empty) restOfPath node

_dump path File{_size=n} = (intercalate "/" path) ++ " " ++ (show n) ++ "\n"
_dump path Dir {_nodes=nodes} = 
    (intercalate "/" path) ++ "/\n" ++
    (concat
    $ map (\(name, node) -> _dump (path ++ [name]) node)
    $ M.toList nodes )

dump x = _dump [] x

sizes fs = sizes1 [] fs

sizes1 :: [String] -> Node -> ([([String], Int)], Int)
sizes1 path (Dir nodes) = (dirsWithSizes, totalSize) where
    dirsWithSizes = ((path, totalSize) : subdirWithSizes) 
    totalSize = filesSize + subdirsSize
    filesSize = sum $ map (_size . snd) $ files
    subdirResults = map (\(name, subdir) -> sizes1 (path ++ [name]) subdir) $ subdirs
    subdirWithSizes = concat $ map fst $ subdirResults
    subdirsSize = sum $ map snd $ subdirResults
    files = filter isFile $ M.toList $ nodes
    subdirs = filter (not . isFile) $ M.toList $ nodes

-- transcript interpreter

data State = State {cwd :: [String], fs :: Node}

initialState = State [] (Dir M.empty)

processLine :: State -> ParsedLine -> State
processLine state CdRoot = state {cwd=[]}
processLine state@State{cwd=cwd} (Cd dir) = state{cwd=cwd++[dir]}
processLine state@State{cwd=cwd} CdUp = state{cwd=take ((length cwd) - 1) cwd}
processLine state Ls = state
processLine state (LDir _) = state
processLine state@State{fs=fs, cwd=cwd} (LFile name size) = state{fs=fs'} where
    fs' = insert fs (cwd ++ [name]) (File size)

-- parse input
parseTranscript :: T.Text -> [ParsedLine]
parseTranscript text =
      map (\(Right parsedLine) -> parsedLine)
    $ map (parseOnly inputLine) 
    $ T.lines 
    $ text

-- interpret transcript and build FS model
transcript2fs :: [ParsedLine] -> Node
transcript2fs lines = 
    fs
    $ foldl processLine initialState lines

part1 text =
    sum
    $ filter (\x -> x<=100000)
    $ map snd -- sizes
    $ fst     -- directories with sizes
    $ sizes
    $ transcript2fs
    $ parseTranscript text

part2 text = minDirSize where
    (dirsWithSizes, totalUsed) = sizes $ transcript2fs $ parseTranscript $ text
    deviceSize = 70000000
    requiredSpace = 30000000
    minToFree = (requiredSpace + totalUsed) - deviceSize
    minDirSize = head $ sort $ filter (\x->x>=minToFree) $ map snd $ dirsWithSizes


main :: IO ()
main = do
    example <- TIO.readFile "07-example.txt"
    input <- TIO.readFile "../data/07.txt"
    putStrLn "part 1"
    putStrLn $ show $ part1 $ example
    putStrLn $ show $ part1 $ input
    putStrLn "part 2"
    putStrLn $ show $ part2 $ example
    putStrLn $ show $ part2 $ input
