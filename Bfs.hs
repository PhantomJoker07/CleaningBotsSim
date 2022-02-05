module Bfs where

import Tools

--Get the positions adjacents to (i,j) of a nxm dimensions matrix (adjacents considering only four directions)
adjacents :: Int -> Int -> Int -> Int -> [(Int, Int)]
adjacents i j n m = [(x,y) | (x,y) <- [(i,j+1),(i,j-1),(i-1,j),(i+1,j)], 0 <= x && x < n && 0 <= y && y < m]

--Find all the valid neighbors adjacents to the position u of the matrix without considering the types specified in blockers
findNeighbors :: Eq a => [[a]] -> [a] -> (Int,Int) -> [(Int,Int)]
findNeighbors matrix blockers u =
    let
        (i,j) = u
        n = length matrix
        m = length (matrix !! 0)
        adj = adjacents i j n m
        bPos = getPositionsInMatrix matrix blockers 0 0
    in
        filter (\x -> not (x `elem` bPos)) adj

--Generate the distances matrix after doing a bfs algorithm in the martix starting in the position u and ignoring the blockers
bfsInMatrix :: Eq a=> [[a]] -> [a] -> (Int,Int) -> [[Int]]
bfsInMatrix mtx blockers u =
    let
        n = length mtx 
        m = length (mtx !! 0)
        (i,j) = u 
        tempDistances = matrix n m (-1)
        distances = setMatrix tempDistances i j 0
    in
        doBfs [] mtx blockers [u] distances

--Bfs algorithm
doBfs :: Eq a => [(Int,Int)] -> [[a]] -> [a] -> [(Int,Int)] -> [[Int]] -> [[Int]]
doBfs _seen mtx blockers queue distance
    | queue == [] = distance
    | otherwise =
        let
            seen = (head queue:_seen)
            neighbors = findNeighbors mtx blockers (head queue)
            (x, y) = head queue
            neighbors_not_see = filter (\x -> not(x `elem` seen)) neighbors
            (_,new_distance,_,_) = for 0 (length neighbors_not_see) 1 (neighbors_not_see, distance, x, y)
                                (\(n,d,u,v) -> let (i,j) = head n in (tail n, setMatrix d i j (((d !! u) !! v) + 1), u, v))

            new_seen = neighbors_not_see ++ seen
            new_queue = (tail queue) ++ neighbors_not_see
        in
            doBfs new_seen mtx blockers new_queue new_distance

--Get the shortest path starting in the first position of path and ending in the starting position of the distances matrix (the one with distance 0)
getShortestPath:: [[Int]] -> [(Int,Int)] -> [(Int,Int)]
getShortestPath distancesMatrix path =
    let
        currentPosition = head path
        (x,y) = currentPosition
        val = (distancesMatrix !! x !! y)
        answer
            | val == 0 = path
            | otherwise =
                let 
                    n = length distancesMatrix
                    m = length (head distancesMatrix)
                    neighbors = adjacents x y n m
                    nexts = filter (\x -> let (_i,_j) = x in (distancesMatrix !! _i !! _j) == (val - 1)) neighbors
                    next = head nexts
                in
                    getShortestPath distancesMatrix (next:path)
    in
         answer 

--Get te index of the minimum value of the distance matrix from the ones located in the list of possible targets
getMinValueIndex ::  [[Int]] -> [(Int,Int)] -> Int -> Int -> Int -> Int
getMinValueIndex distancesMatrix possibilities indexPos min indexMin =
    let
        (_i,_j) = possibilities !! indexPos
        val = distancesMatrix !! _i !! _j
        (newMin,newIndexMin)
            | not (min == -1) && min <= val = (min,indexMin)
            | otherwise = (val,indexPos)
        ans
            | indexPos == 0 = newIndexMin
            | otherwise = getMinValueIndex distancesMatrix possibilities (indexPos-1) newMin newIndexMin
    in
        ans

--Get the closest target to the starting position of the distances matrix from a list of possible targets
getClosestTarget :: [[Int]] -> [(Int,Int)] -> (Int,Int)
getClosestTarget distancesMatrix possibilities =
    let
        avaliablePos = filter (\x -> let (i,j) = x in not ((distancesMatrix !! i !! j) == -1)) possibilities
        indexPos = length avaliablePos - 1 
        indexMin
            | not (indexPos == -1) = getMinValueIndex distancesMatrix avaliablePos indexPos (-1) (-1)
            | otherwise = -1
        target
            | not (indexMin == -1) = avaliablePos !! indexMin
            | otherwise = (-1,-1)
    in 
        target

--Get the next position of the path considering the existence of an step size while traversing said path
getNextCell :: [(Int,Int)] -> Int -> (Int,Int)
getNextCell path stepSize =
    let
        pathLength = length path
        pos
            | stepSize <  pathLength = stepSize
            | otherwise =  pathLength - 1
    in    
        path !! pos