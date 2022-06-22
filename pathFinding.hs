
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

grid2 = ["S..",".O.","..E"]
grid3 = ["S...",
         ".O..",
         "..O.",
         "...E"]


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


-- get the number of the steps
numLen :: ([(Int,Int)],Int) -> Int
numLen (_,n) = n

-- return the path which has the least steps
getBestPath :: [([(Int,Int)],Int)] -> ([(Int,Int)],Int)
getBestPath [] = ([],100)
getBestPath (st : sts) | numLen st < numLen st' = st
                       | otherwise = st'
    where
        st' = getBestPath sts


-- find the best path on a grid
bestPath :: [[Char]] -> (Int,Int) -> ([(Int, Int)],Int)
bestPath xs (x,y) = getBestPath (paths xs ([], 0) (x,y))


-- function to get all the possible paths from a position
paths :: [[Char]] -> ([(Int,Int)],Int) -> (Int,Int) -> [([(Int,Int)],Int)]
-- xs->grid, s1->current path, (x,y)->current position
paths xs (s1, len) (x,y) | getAtPos xs (x, y) == 'E' = [(s1, len)] -- if the current position is the end
                         | otherwise = concat[paths (blockPos xs (x, y)) 
                         (s1++[(x,y)], len + 1) (c)| c <- legalPos xs (x,y)]
                         -- Every time pass a point, turn that point into a wall



