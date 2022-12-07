import Data.List (tails, nub)

detect win input = 
    ((+)win)
    $ fst
    $ head
    $ filter  (\(n, suffix) -> ((win)==) $ length $ nub $ take win $ suffix)
    $ zip [0..]
    $ tails
    $ input

part1 = detect 4

part2 = detect 14

main :: IO ()
main = do
    putStrLn "part1"
    putStrLn . show $ part1 $ "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
    putStrLn . show $ part1 $ "bvwbjplbgvbhsrlpgdmjqwftvncz"
    putStrLn . show $ part1 $ "nppdvjthqldpwncqszvftbrmjlhg"
    putStrLn . show $ part1 $ "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
    putStrLn . show $ part1 $ "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
    input <- readFile "06.txt"
    putStrLn . show $ part1 $ input
    putStrLn "part2"
    putStrLn . show $ part2 $ "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
    putStrLn . show $ part2 $ "bvwbjplbgvbhsrlpgdmjqwftvncz"
    putStrLn . show $ part2 $ "nppdvjthqldpwncqszvftbrmjlhg"
    putStrLn . show $ part2 $ "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
    putStrLn . show $ part2 $ "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
    putStrLn . show $ part2 $ input
