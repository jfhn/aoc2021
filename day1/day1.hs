countIncreases :: Int -> [Int] -> Int
countIncreases _    []     = 0
countIncreases (-1) (x:xs) = countIncreases x xs
countIncreases last (x:xs) = (if last < x then 1 else 0) + (countIncreases x xs)

solve1 :: [String] -> Int
solve1 = countIncreases (-1) . getInput

sumWindows :: Int -> Int -> [Int] -> [Int]
sumWindows _    _    []     = []
sumWindows (-1) (-1) (a:xs) = sumWindows a (-1) xs
sumWindows a    (-1) (b:xs) = sumWindows a b xs
sumWindows a    b    (c:xs) = (a + b + c) : sumWindows b c xs

solve2 :: [String] -> Int
solve2 = countIncreases (-1) . sumWindows (-1) (-1) . getInput

getInput :: [String] -> [Int]
getInput lines = map (\line -> read line :: Int) lines

main :: IO ()
main = do
    lines <- fmap lines $ readFile "input.txt"
    print $ solve2 lines
