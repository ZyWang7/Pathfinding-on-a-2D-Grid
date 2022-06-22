
-- defined the grid
grid = ["S.........", 
        "..........",
        "..........",
        "..........",
        "..........",
        "..........",
        "..........",
        "..........",
        "..........",
        ".........E"]

grid' = blockPos grid (9,7)
grid'' = blockPos grid' (8,7)
grid''' = blockPos grid'' (6,7)
grid1 = blockPos grid''' (6,8)


-- change the value of a list in the given index position to a new value
setAtIndex :: [a] -> Int -> a -> [a]
setAtIndex xs m n = if m == 0 then [n] ++ tail xs else
                    (head xs) : setAtIndex (tail xs) (m-1) n

-- function to change the given position to obstacles
blockPos :: [[Char]] -> (Int,Int) -> [[Char]]
blockPos xs (x,y) | x == 0 = (setAtIndex (head xs) y 'O') : (tail xs)
                  | otherwise = (head xs) : blockPos (tail xs) (x-1,y)



