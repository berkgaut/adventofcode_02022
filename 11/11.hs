{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Applicative ((<|>))
import qualified Data.Vector as V (Vector, fromList, toList, length)
import Data.Vector ((//), (!))
import Data.List (iterate, sortOn, intercalate)

import Data.Attoparsec.Text (
    Parser,
    char,
    decimal,
    endOfLine,
    parseOnly,
    sepBy,
    space,
    string)
    
import Data.List (singleton, concatMap)
import Data.Char (ord)
import Data.Maybe (listToMaybe)

--- Parse Input ---

data Monkey = Monkey {
    _monkeyId :: MonkeyId,
    _items :: [Item],
    _operation :: Expr,
    _test :: Test,
    _ifTrue  :: Throw,
    _ifFalse :: Throw,
    _inspections :: Int
} deriving Show
type MonkeyId = Int
type Item = Int
data Expr = Binary { op :: Operator, left, right :: Term } deriving Show
data Operator = Add | Mul deriving Show
data Term = Old | Lit Item deriving Show
data Test = DivisibleBy {_divisor::Item} deriving Show
data Throw = ThrowTo {_throwToMonkedId :: MonkeyId} deriving Show

input = monkey `sepBy` endOfLine >>= return . V.fromList
monkey = do
    string "Monkey "
    monkeyId <- decimal
    char ':'
    endOfLine
    --
    string "  Starting items: "
    startingItems <- decimal `sepBy` (string ", ")
    endOfLine
    --
    string "  Operation: new = "
    term1 <- term
    space
    operator <- operator
    space
    term2 <- term
    endOfLine
    --
    string "  Test: divisible by "
    divisor <- decimal
    endOfLine
    --
    string "    If true: "
    ifTrue <- throw
    endOfLine
    string "    If false: "
    ifFalse <- throw
    endOfLine
    return $ Monkey {_monkeyId=monkeyId, _items=startingItems, _operation=Binary operator term1 term2, _test=DivisibleBy divisor, _ifTrue=ifTrue, _ifFalse=ifFalse, _inspections=0}

term = (string "old" >> return Old) <|> (decimal >>= return . Lit)
operator = (char '+' >> return Add) <|> (char '*' >> return Mul)
throw = (string "throw to monkey " >> decimal >>= return . ThrowTo)

parseInput t = case (parseOnly input) t of
    (Right result) -> result

-- Interpret input

evalE :: Expr -> Item -> Item
evalE (Binary Add t1 t2) old = (evalT t1 old) + (evalT t2 old)
evalE (Binary Mul t1 t2) old = (evalT t1 old) * (evalT t2 old)

evalT :: Term -> Item -> Item
evalT Old old = old
evalT (Lit n) _ = n

type LimitF = Item->Item
bored :: Item -> Item
bored n = floor $ (fromIntegral n) / 3

inspectAllItems :: LimitF -> Monkey -> (Monkey, [(MonkeyId, Item)])
inspectAllItems limitF monkey = (monkey', throws) where
    items = _items monkey
    monkey' = monkey{_items=[], _inspections=(length items) + _inspections monkey}
    throws = map inspectItem items
    inspectItem item = throw $ test (_test monkey) $ limitF $ evalE (_operation monkey) $ item
    test (DivisibleBy divisor) item = ((item `mod` divisor) == 0, item)
    throw (testResult, item) = (_throwToMonkedId $ (if testResult then _ifTrue else _ifFalse) $ monkey, item)

catchItem :: V.Vector Monkey -> (MonkeyId, Item) -> V.Vector Monkey
catchItem monkeys (id, item) = monkeys // [(id, monkey')] where
    monkey = monkeys ! id
    monkey' = monkey { _items = _items monkey ++ [item] }

catchAll :: V.Vector Monkey -> [(MonkeyId, Item)] -> V.Vector Monkey
catchAll monkeys throws = foldl catchItem monkeys throws

step :: LimitF -> V.Vector Monkey -> Int -> V.Vector Monkey
step limitF monkeys id = catchAll monkeys' throws where
    (monkey', throws) = inspectAllItems limitF $ monkeys ! id
    monkeys' = monkeys // [(id, monkey')]

rround :: LimitF -> V.Vector Monkey -> V.Vector Monkey
rround limitF monkeys = foldl (step limitF) monkeys [0 .. V.length monkeys - 1]

--- Part 1 ---

run limitF rounds monkeys = 
   (\(m1:(m2:_))->_inspections m1 * _inspections m2)
   $ reverse
   $ sortOn _inspections
   $ V.toList
   $ last
   $ take (rounds + 1)
   $ iterate (rround limitF)
   $ monkeys

part1 t = run bored 20 $ parseInput t

--- Part 2 ---

part2 t = run (\item->item `mod` magic) 10000 monkeys where
    monkeys = parseInput t
    magic = foldl (*) 1 $ map (_divisor . _test) $ V.toList $ monkeys

main :: IO ()
main = do
    example <- TIO.readFile "11-example.txt"
    input <- TIO.readFile "../data/11.txt"
    putStrLn "--- Part 1 ---"
    putStrLn $ show $ part1 $ example
    putStrLn $ show $ part1 $ input
    putStrLn "--- Part 2 ---"
    putStrLn $ show $ part2 $ example
    putStrLn $ show $ part2 $ input
