data Move = Forward | Down | Up

calcPos :: (Int, Int) -> (Move, Int) -> (Int, Int)
calcPos (x, y) (Forward, d) = (x + d, y)
calcPos (x, y) (Down, d)    = (x, y + d)
calcPos (x, y) (Up, d)      = (x, y - d)

prepareCommand :: [String] -> (String, Int)
prepareCommand cmd
    | cmd !! 0 == "forward" = ("forward", read (cmd !! 1) :: Int)
    | cmd !! 0 == "down"    = ("down", read (cmd !! 1) :: Int)
    | cmd !! 0 == "up"      = ("up", read (cmd !! 1) :: Int)

getCommand :: (String, Int) -> (Move, Int)
getCommand ("forward", d) = (Forward, d)
getCommand ("down"   , d) = (Down, d)
getCommand ("up"     , d) = (Up, d)

solve1 :: [String] -> Int
solve1 = (\(x, y) -> x * y) . foldl calcPos (0, 0) . map (\s -> getCommand $ prepareCommand $ words s)

calcPos2 :: (Int, Int, Int) -> (Move, Int) -> (Int, Int, Int)
calcPos2 (x, y, aim) (Forward, d) = (x + d, y + (aim * d), aim)
calcPos2 (x, y, aim) (Down, d)    = (x, y, aim + d)
calcPos2 (x, y, aim) (Up, d)      = (x, y, aim - d)

solve2 :: [String] -> Int
solve2 = (\(x, y, _) -> x * y) . foldl calcPos2 (0, 0, 0) . map (\s -> getCommand $ prepareCommand $ words s)

main :: IO ()
main = do
    lines <- fmap lines $ readFile "input.txt"
    print $ solve2 lines
