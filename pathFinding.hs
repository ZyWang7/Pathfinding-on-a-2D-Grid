
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


-- change the value of a list in the given index position to a new value
setAtIndex :: [a] -> Int -> a -> [a]
setAtIndex xs m n = if m == 0 then [n] ++ tail xs else
                    (head xs) : setAtIndex (tail xs) (m-1) n

-- function to change the given position to obstacles
blockPos :: [[Char]] -> (Int,Int) -> [[Char]]
blockPos xs (x,y) | x == 0 = (setAtIndex (head xs) y 'O') : (tail xs)
                  | otherwise = (head xs) : blockPos (tail xs) (x-1,y)


grid' = blockPos grid (9,7)
grid'' = blockPos grid' (8,7)
grid''' = blockPos grid'' (6,7)
grid1 = blockPos grid''' (6,8)


-- confirm whether the coordinate position (x,y) is legal or not
posOnGrid :: [[Char]] -> (Int,Int) -> Bool
posOnGrid vss (x,y) = x >= 0 && x < length(vss)
                    && y >= 0 && y < length (vss !! x)

-- return the value of item at position (x,y)
getAtPos :: [[Char]] -> (Int,Int) -> Char
getAtPos vss (x,y) | posOnGrid vss (x,y) = (vss!!x)!!y
                   | otherwise = 'O'

-- function to check the position is legal or not
isLegalPos :: [[Char]] -> (Int,Int) -> Bool
isLegalPos xs (x,y) = if getAtPos xs (x,y) == 'O' then False
                    else True


-- function to get the legal positions from a position
legalPos :: [[Char]] -> (Int,Int) -> [(Int, Int)]
legalPos xs (x,y) = [c | c <- [(x-1,y-1), (x-1,y), (x-1,y+1), (x,y-1), (x,y+1), (x+1,y-1), (x+1,y), (x+1, y+1)],
                     isLegalPos xs c == True]

